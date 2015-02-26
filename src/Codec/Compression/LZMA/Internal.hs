{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Codec.Compression.LZMA.Internal
  (
  -- * Decompression
    DecompressParams(..)
  , defaultDecompressParams

  -- ** Lazy 'L.ByteString's
  , decompress

  -- ** Incremental processing
  -- *** Sequential decompression
  , DecompressStream
  , decompressST
  , decompressIO
  , decompressStream
  -- *** Decompression with random access support
  , SeekableDecompressStream
  , seekableDecompressIO
  , seekableDecompressStream
  -- *** Decoding indicies
  , decodeIndicies
  , DecodeStream
  , decodeIndexStream

  -- * Types for incremental processing
  , ReadRequest(..)
  , Position
  , Compression(..)
  , Size

  -- * Exceptions
  , SomeLZMAException(..)
  , DecompressException(..)
  , DecodeException(..)
  ) where
import Control.Applicative
import Control.Exception (IOException, assert)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Word
import Foreign hiding (void)
import System.IO

import Control.Monad.Catch
import Control.Monad.Trans
import Pipes hiding (next, void)
import Pipes.Core
import qualified Control.Monad.ST as S
import qualified Control.Monad.ST.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Pipes.Internal as P

import Codec.Compression.LZMA.Internal.IndexDecoder (IndexDecoder)
import Codec.Compression.LZMA.Internal.Stream (Stream)
import Codec.Compression.LZMA.Internal.Types
import qualified Codec.Compression.LZMA.Internal.C as C
import qualified Codec.Compression.LZMA.Internal.IndexDecoder as ID
import qualified Codec.Compression.LZMA.Internal.Stream as Stream

#if DEBUG
import Debug.Trace
#endif

-- | The full set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
data DecompressParams = DecompressParams
  { decompressBufferSize :: !Int
  -- ^ The size of the first output buffer, containing the uncompressed data.
  -- If you know an exact or approximate upper bound on the size of the
  -- decompressed data then setting this parameter can save memory. The default
  -- decompression output buffer size is 32k. If your estimate is wrong it does
  -- not matter too much, the default buffer size will be used for the remaining
  -- chunks.
  --
  -- One paticular use case for setting the 'decompressBufferSize' is if you
  -- know the exact size of the decompressed data and want to produce a strict
  -- 'S.ByteString'. The compression and decompression function use lazy
  -- 'L.ByteString's but if you set the 'decompressBufferSize' correctly then
  -- you can generate a lazy 'L.ByteString' with exactly one chunk, which can
  -- be converted to a strict 'S.ByteString' in @O(1)@ time using
  -- @'S.concat' . 'L.toChunks'@.
  , decompressMemoryLimit :: !Word64
  -- ^
  }

-- | The default set of parameters for decompresssion. This is typically used
-- with the 'decompressWith' function with specific parameters overridden.
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams
  { decompressBufferSize = defaultDecompressBufferSize
  , decompressMemoryLimit = maxBound -- No limit
  }

-- defaultCompressBufferSize :: Int
-- defaultCompressBufferSize = 16 * 1024 - L.chunkOverhead

defaultDecompressBufferSize :: Int
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead

-- | The unfolding of the decompression process, where you provide a sequence
-- of compressed data chunks as input and receive a sequence of uncompressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
type DecompressStream = Pipe S.ByteString S.ByteString

handleRet
  :: (MonadTrans t, Monad (t m), MonadThrow m)
  => t m C.Ret
  -> t m ()
handleRet m = do
  ret <- m
  case ret of
    C.Error errorCode -> lift $ throwDecompressError errorCode
    _ -> return ()

throwDecompressError :: MonadThrow m => C.ErrorCode -> m a
throwDecompressError = throwM . DecompressError

-- | The possible error cases when decompressing a stream.
data DecompressException = DecompressError Stream.ErrorCode
  deriving (Eq, Show, Typeable)

instance Exception DecompressException where
  toException = Stream.lzmaExceptionToException
  fromException = Stream.lzmaExceptionFromException

decompress :: DecompressParams -> L.ByteString -> L.ByteString
decompress = decompressStreamToLBS . decompressStream

decompressST :: DecompressParams -> DecompressStream (L.ST s) ()
decompressST = decompressStreamToST . decompressStream

decompressIO :: DecompressParams -> DecompressStream IO ()
decompressIO = decompressStreamToIO . decompressStream

decompressStream :: DecompressParams -> DecompressStream Stream ()
decompressStream params = do
  handleRet $ lift $ Stream.autoDecoder (decompressMemoryLimit params) mempty
  loop
  where
    loop = fillBuffers params () >>= drainBuffers
    drainBuffers isLastChunk = do
      lift $ assertBuffers isLastChunk

      res <- lift $ Stream.decompress $ if isLastChunk
        then Stream.Finish
        else Stream.Run

      case res of
        Stream.Ok -> do
          outputBufferFull <- lift Stream.isOutputBufferFull
          when outputBufferFull $ do
            -- write out if the output buffer became full
            (outFPtr, outOffset, outLen) <- lift Stream.popOutputBuffer
            yield $ S.PS outFPtr outOffset outLen
          loop
        Stream.StreamEnd -> do
          inputBufferEmpty <- lift Stream.isInputBufferEmpty
          remaining <- if inputBufferEmpty
            then return S.empty
            else do
              (inFPtr, inOffset, inLen) <- lift Stream.remainingInputBuffer
              return $ S.PS inFPtr inOffset inLen
          void $ finalizeStream 0
          yield remaining
        Stream.Error errorCode -> do
          void $ finalizeStream 0
          lift $ throwDecompressError errorCode

------------------------------------------------------------

decompressStreamToLBS
  :: DecompressStream Stream a -> L.ByteString -> L.ByteString
decompressStreamToLBS stream input = L.runST $ do
  state <- L.strictToLazyST Stream.newState
  go stream state input
  where
    go (P.Request _ next) state inChunks =
      case inChunks of
        L.Empty ->
          go (next S.empty) state L.Empty
        L.Chunk chunk chunks ->
          go (next chunk) state chunks
    go (P.Respond outChunk next) state inChunks = do
      outChunks <- go (next ()) state inChunks
      return $ L.Chunk outChunk outChunks
    go (P.M m) state inChunks = do
      (next, state') <- L.strictToLazyST $ Stream.runStream m state
      go next state' inChunks
    go (P.Pure _) _ !_inChunks = return L.Empty

decompressStreamToST
  :: Proxy x' x y' y Stream a
  -> Proxy x' x y' y (L.ST s) a
decompressStreamToST stream = do
  state <- lift $ L.strictToLazyST Stream.newState
  go stream state
  where
    go (P.Request req next) state = do
      chunk <- request req
      go (next chunk) state
    go (P.Respond chunk next) state = do
      res <- respond chunk
      go (next res) state
    go (P.M m) state = do
      (next, state') <- lift $ L.strictToLazyST $ Stream.runStream m state
      go next state'
    go (P.Pure chunk) _ = return chunk

decompressStreamToIO
  :: Proxy x' x y' y Stream a
  -> Proxy x' x y' y IO a
decompressStreamToIO stream = do
  state <- lift $ S.stToIO Stream.newState
  go stream state
  where
    go (P.Request req next) state = do
      chunk <- request req
      go (next chunk) state
    go (P.Respond chunk next) state = do
      res <- respond chunk
      go (next res) state
    go (P.M m) state = do
      (next, state') <- lift $ S.stToIO $ Stream.runStream m state
      go next state'
    go (P.Pure chunk) _ = return chunk

------------------------------------------------------------

-- | The unfolding of the decompression process with random seek support.
--
-- Downstream demands uncompressed bytes from a specific position using
-- 'ReadRequest's. 'SeekableDecompressStream' translates them to compressed
-- position using 'Index' and sends them to upstream.
type SeekableDecompressStream = Proxy
  (ReadRequest 'Compressed) S.ByteString
  (ReadRequest 'Uncompressed) S.ByteString

seekableDecompressIO
  :: DecompressParams
  -> C.Index
  -- ^ Index of the stream
  -> ReadRequest 'Uncompressed
  -- ^ Initial request
  -> SeekableDecompressStream IO ()
seekableDecompressIO params index =
  decompressStreamToIO . seekableDecompressStream params index

seekableDecompressStream
  :: DecompressParams
  -> C.Index
  -- ^ Index for the stream
  -> ReadRequest 'Uncompressed
  -- ^ Initial request
  -> SeekableDecompressStream Stream ()
seekableDecompressStream params index req0 = do
  iter <- liftIO $ C.lzma_index_iter_init index
  decodeLoop iter req0
  liftIO $ C.touchIndex index
  where
    decodeLoop iter req = do
      found <- locateBlock iter req
      when found $ do
        req' <- decodeBlock iter req
        decodeLoop iter req'

    -- | Decode a new block
    decodeBlock
      :: C.IndexIterPtr
      -> ReadRequest 'Uncompressed
      -> SeekableDecompressStream Stream (ReadRequest 'Uncompressed)
    decodeBlock iter req = do
      lift Stream.flushBuffers
      indexIter@C.IndexIter {..} <- liftIO $ withForeignPtr iter peek
      let
        C.IndexIterStream {indexIterStreamBlockCount} = indexIterStream
        C.IndexIterBlock {..} = indexIterBlock
        blockPos :: Position 'Compressed
        blockPos = fromIntegral indexIterBlockCompressedFileOffset
        blockUncompPos :: Position 'Uncompressed
        blockUncompPos = fromIntegral indexIterBlockUncompressedFileOffset
      -- Check if the block number doesn't exceed the total block count.
      assert (indexIterBlockNumberInStream <= indexIterStreamBlockCount) $
        return ()
      isLastChunk <- fillBuffers params $ PReadWithSize
        blockPos
        (fromIntegral indexIterBlockTotalSize)
      block <- liftIO C.newBlock
      filters <- liftIO C.newFiltersMaxLength
      handleRet $ lift $ Stream.blockDecoder indexIter block filters

      req' <- drainBuffers iter isLastChunk
        (calculateSkipBytes req blockUncompPos)

      liftIO $ do
        -- The filters and the block need to be kept in memory until a whole
        -- block is decoded. They're not touched in Haskell but are used in
        -- liblzma during decoding.
        C.touchFilters filters
        C.touchBlock block
      return req'

    calculateSkipBytes
      :: ReadRequest 'Uncompressed -> Position 'Uncompressed -> Int
    calculateSkipBytes req blockPos = case req of
      PRead pos -> fromIntegral (pos - blockPos)
      Read -> 0

    fillBuffers'
      :: C.IndexIterPtr
      -> ReadRequest 'Compressed
      -> Int -- ^ Offset from the beginning of the block to the target position
      -> SeekableDecompressStream Stream (ReadRequest 'Uncompressed)
    fillBuffers' iter req skipBytes = do
      isLastChunk <- fillBuffers params req
      drainBuffers iter isLastChunk skipBytes

    drainBuffers
      :: C.IndexIterPtr
      -> Bool -- ^ Last chunk or not
      -> Int -- ^ Offset from the beginning of the block to the target position
      -> SeekableDecompressStream Stream (ReadRequest 'Uncompressed)
    drainBuffers iter isLastChunk skipBytes = do
      lift $ assertBuffers isLastChunk

      ret <- lift $ Stream.decompress $ if isLastChunk
        then Stream.Finish
        else Stream.Run
      case ret of
        Stream.Ok -> do
          outputBufferFull <- lift Stream.isOutputBufferFull
          if outputBufferFull
            then do
              (outFPtr, outOffset, outLen) <- lift Stream.popOutputBuffer
              if outLen <= skipBytes
                then fillBuffers' iter Read (skipBytes - outLen)
                else do
                  req <- respond $ S.PS outFPtr
                    (outOffset + skipBytes)
                    (outLen - skipBytes)
                  case req of
                    PRead _pos -> return req
                    Read -> fillBuffers' iter Read 0
            else
              fillBuffers' iter Read skipBytes

        Stream.StreamEnd -> do
          inputBufferEmpty <- lift Stream.isInputBufferEmpty
          leftover <- if inputBufferEmpty
            then return S.empty
            else do
              (inFPtr, inOffset, inLen) <- lift Stream.remainingInputBuffer
              return $ S.PS inFPtr inOffset inLen
          req'm <- finalizeStream skipBytes
          return $ fromMaybe Read req'm

        Stream.Error errorCode -> do
          void $ finalizeStream skipBytes
          lift $ throwDecompressError errorCode

-- | If the 'ReadRequest' has a position, find the block which contains the
-- position. If it doesn't have a position, find the next non-empty block.
--
-- This function return true when the block is found.
locateBlock
  :: C.IndexIterPtr
  -> ReadRequest 'Uncompressed
  -> SeekableDecompressStream Stream Bool
locateBlock iter req = not <$> liftIO act
  where
    act = case req of
      -- If the request has a position, decode the block which contains the
      -- position.
      PRead pos -> C.lzma_index_iter_locate iter (fromIntegral pos)
      -- If the request doesn't have a position, continue reading bytes from
      -- the current position.
      Read -> C.lzma_index_iter_next iter C.IndexIterNonEmptyBlockMode

fillBuffers :: DecompressParams -> r -> Proxy r S.ByteString y' y Stream Bool
fillBuffers DecompressParams {..} req = do
#ifdef DEBUG
  lift $ Stream.consistencyCheck
#endif
  fillOutputBuffer
  fillInputBuffer
  where
    fillOutputBuffer = lift $ do
      outputBufferFull <- Stream.isOutputBufferFull
      when outputBufferFull $ do
        outFPtr <- liftIO $ S.mallocByteString decompressBufferSize
        Stream.pushOutputBuffer outFPtr 0 decompressBufferSize

    fillInputBuffer = do
      inputBufferEmpty <- lift Stream.isInputBufferEmpty
      if inputBufferEmpty
        then do
          chunk <- request req
          case chunk of
            _ | S.null chunk -> return True
            S.PS inFPtr inOffset inLen -> do
              lift $ Stream.pushInputBuffer inFPtr inOffset inLen
              return False
        else return False

-- | Sanity checks for buffers
assertBuffers :: Bool -> Stream ()
assertBuffers isLastChunk = do
  inputBufferEmpty <- Stream.isInputBufferEmpty
  outputBufferFull <- Stream.isOutputBufferFull
  let isSane = not outputBufferFull && (isLastChunk || not inputBufferEmpty)
  assert isSane $ return ()

finalizeStream :: Int -> Proxy x' x y' S.ByteString Stream (Maybe y')
finalizeStream skipBytes = do
  outputBufferBytesAvailable <- lift Stream.outputBufferBytesAvailable
  if outputBufferBytesAvailable > skipBytes
    then do
      (outFPtr, outOffset, outLen) <- lift Stream.popOutputBuffer
      lift Stream.end
      req <- respond $ S.PS outFPtr (outOffset + skipBytes) (outLen - skipBytes)
      return $ Just req
    else do
      lift Stream.end
      return Nothing

-----------------------------------------------------------
-- Index decoder

decodeIndicies :: Handle -> IO (C.Index, C.VLI)
decodeIndicies h = do
  size <- hFileSize h
  C.allocaStreamFlags $ \header ->
    C.allocaStreamFlags $ \footer ->
      runDecodeStream h $ indexDecodingToIO
        (decodeIndexStream (fromIntegral size))
        ID.newIndexDecoderState header footer

runDecodeStream
  :: (MonadIO m, MonadThrow m)
  => Handle
  -> DecodeStream m a
  -> m a
runDecodeStream h = runEffect . loop
  where
    loop (P.Request seekRequest next) = do
      size <- case seekRequest of
        PReadWithSize (fromIntegral -> pos) size -> do
          r <- liftIO $ try $ hSeek h AbsoluteSeek pos
          case r of
            Left e -> lift $ throwM (e :: IOException)
            Right () -> return size
        _ -> return L.defaultChunkSize
      chunk <- liftIO $ hGetEnsureN h size
      loop (next chunk)
    loop (P.Respond _ next) = loop (next ())
    loop (P.M m) = lift m >>= loop
    loop (P.Pure a) = return a

hGetEnsureN :: Handle -> Int -> IO S.ByteString
hGetEnsureN h expectedLen = do
  fp <- S.mallocByteString expectedLen
  withForeignPtr fp $ \p -> do
    actualLen <- hGetBuf h p expectedLen
    if expectedLen <= actualLen
      then return $ S.PS fp 0 expectedLen
      else fail $ "hGetEnsureN: unexpected end of file (demanded " ++
        show expectedLen ++ " bytes, but got " ++ show actualLen ++
        " bytes)"

-- | Seek to an absolute position and ask for an input with given size.
pread
  :: Size
  -- ^ Input size
  -> DecodeStream IndexDecoder S.ByteString
pread size = do
  pos <- lift ID.getPosition
  request $ PReadWithSize pos size

-- | Decode things from compressed stream.
type DecodeStream = Client (ReadRequest 'Compressed) S.ByteString

-- | Seek operation failure in downstream.
data DecodeException = DecodeError C.ErrorCode deriving (Show, Typeable)

instance Exception DecodeException where
  toException = lzmaExceptionToException
  fromException = lzmaExceptionFromException

------------------------------------------------------------

headerSize, footerSize :: Integral a => a
headerSize = fromIntegral C.streamHeaderSize
footerSize = headerSize

-- | Seek thorough a seekable stream and build a combined index. The index can
-- later be used for seeking.
decodeIndexStream
  :: Size -- ^ Size of the file
  -> DecodeStream IndexDecoder (C.Index, C.VLI)
decodeIndexStream fileSize = do
  lift $ ID.setPosition $ fromIntegral fileSize
  index <- parseIndex
  loop index
  padding <- lift ID.getStreamPadding
  return (index, padding)
  where
    loop :: C.Index -> DecodeStream IndexDecoder ()
    loop index = do
      pos <- lift ID.getPosition
      when (pos > 0) $ do
        index' <- parseIndex
        handleRet $ liftIO $ C.lzma_index_cat index index'
        loop index

-- | Parse an index
parseIndex :: DecodeStream IndexDecoder C.Index
parseIndex = do
  padding <- decodeStreamFooter
  index <- decodeIndex 8192 -- FIXME: Set appropreate size
  decodeStreamHeader index
  checkIntegrity index
  handleRet $ liftIO $ C.lzma_index_stream_padding index padding
  lift $ ID.modifyStreamPadding' (+ padding)
  return index

decodeStreamFooter :: DecodeStream IndexDecoder C.VLI
decodeStreamFooter = loop 0
  where
    loop padding = do
      pos <- lift ID.getPosition
      when (pos < 2 * headerSize) $
        lift $ throwM $ DecodeError C.DataError

      pos' <- lift $ do
        ID.modifyPosition' (subtract footerSize)
        ID.getPosition
      when (pos' < footerSize) $
        lift $ throwM $ DecodeError C.DataError

      chunk <- pread footerSize
      if isStreamPadding $ dropStreamPadding 2 chunk
        then do
          padding' <- lift $ skipStreamPaddings chunk 2 padding
          loop padding'
        else do
          footer <- lift ID.getStreamFooter
          handleRet $ liftIO $ do
            let S.PS inFPtr _off _len = chunk
            withForeignPtr inFPtr $ C.lzma_stream_footer_decode footer
          version <- liftIO $ C.lzma_get_stream_flags_version footer
          unless (version ==  0) $
            lift $ throwM $ DecodeError C.DataError
          return padding

skipStreamPaddings
  :: S.ByteString
  -- ^ Input chunk
  -> Int
  -- ^ Index
  -> C.VLI
  -> IndexDecoder C.VLI
skipStreamPaddings chunk = go
  where
    go !i !padding =
      if i >= 0 && isStreamPadding (dropStreamPadding i chunk)
        then do
          ID.modifyPosition' (subtract 4)
          go (i - 1) (padding + 4)
        else return padding

isStreamPadding :: S.ByteString -> Bool
isStreamPadding = S.all (== 0) . S.take 4

dropStreamPadding :: Int -> S.ByteString -> S.ByteString
dropStreamPadding n = S.drop (4*n)

getIndexSize :: IndexDecoder C.VLI
getIndexSize = do
  footer <- ID.getStreamFooter
  liftIO $ C.lzma_stream_flags_backward_size footer

-- | Decode a stream index.
decodeIndex
  :: C.VLI -- ^ Buffer size
  -> DecodeStream IndexDecoder C.Index
decodeIndex bufSize = do
  indexSize <- lift getIndexSize
  -- Set posision to the beginning of the index.
  lift $ ID.modifyPosition' $ subtract $ fromIntegral indexSize
  stream <- liftIO C.newStream
  (ret, indexRef) <- liftIO $ C.lzma_index_decoder stream maxBound -- FIXME: Set proper value
  unless (ret == C.Ok) $ lift $ throwM $ DecodeError C.ProgError

  loop stream indexRef indexSize

  liftIO $ C.peekIndexRef indexRef
  where
    loop stream indexRef indexSize = do
      let inAvail :: Integral a => a
          inAvail = fromIntegral $ min bufSize indexSize
      liftIO $ C.lzma_set_stream_avail_in stream inAvail
      chunk <- pread inAvail
      lift $ ID.modifyPosition' (+ inAvail)
      let indexSize' = indexSize - inAvail
      liftIO $ do
        let S.PS inFPtr _offset _len = chunk
        withForeignPtr inFPtr $ C.lzma_set_stream_next_in stream
      ret <- liftIO $ C.lzma_code stream C.Run
      case ret of
        C.Ok -> loop stream indexRef indexSize'
        C.Error reason -> lift $ throwM $ DecodeError reason
        C.StreamEnd -> do
          inAvail' <- liftIO $ C.lzma_get_stream_avail_in stream
          unless (indexSize' == 0 && inAvail' == 0) $
            lift $ throwM $ DecodeError C.DataError

-- | Decode the stream header and check that its stream flags match the stream
-- footer.
decodeStreamHeader
  :: C.Index
  -> DecodeStream IndexDecoder ()
decodeStreamHeader index = do
  indexSize <- lift getIndexSize
  lift $ ID.modifyPosition' (subtract $ fromIntegral indexSize + headerSize)
  blocksSize <- liftIO $ C.lzma_index_total_size index
  lift $ ID.modifyPosition' (subtract $ fromIntegral blocksSize)
  chunk <- pread headerSize
  let S.PS inFPtr _off _len = chunk
  header <- lift ID.getStreamHeader
  handleRet $ liftIO $ withForeignPtr inFPtr $ C.lzma_stream_header_decode header

checkIntegrity
  :: C.Index
  -> DecodeStream IndexDecoder ()
checkIntegrity index = do
  header <- lift ID.getStreamHeader
  footer <- lift ID.getStreamFooter
  handleRet $ liftIO $ C.lzma_stream_flags_compare header footer
  handleRet $ liftIO $ C.lzma_index_stream_flags index footer
  padding <- lift ID.getStreamPadding
  handleRet $ liftIO $ C.lzma_index_stream_padding index padding

indexDecodingToIO
  :: DecodeStream IndexDecoder a
  -> ID.IndexDecoderState
  -> C.StreamFlags -- ^ Stream header
  -> C.StreamFlags -- ^ Stream footer
  -> DecodeStream IO a
indexDecodingToIO stream0 state0 header footer = go stream0 state0
  where
    go (P.Request req next) state =
      P.Request req $ \chunk -> go (next chunk) state
    go (P.Respond out next) state =
      P.Respond out $ \resp -> go (next resp) state
    go (P.M m) state = do
      (state', stream') <- liftIO $ ID.runIndexDecoder m state header footer
      go stream' state'
    go (P.Pure a) _ =  P.Pure a
