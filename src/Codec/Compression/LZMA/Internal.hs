{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Codec.Compression.LZMA.Internal
  (
  -- * Compression
    CompressParams(..)
  , defaultCompressParams
  , C.Preset
  , C.defaultPreset
  , C.extremePreset
  , C.customPreset
  , C.Check(..)

  -- ** Lazy 'L.ByteString's
  , compress

  -- ** Incremental processing
  , CompressStream
  , compressST
  , compressIO
  , compressStream

  -- * Decompression
  , DecompressParams(..)
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
  , decodeIndex
  , DecodeStream
  , runDecodeStream
  , decodeIndexIO
  , decodeIndexStream

  -- * Types for incremental processing
  , ReadRequest(..)
  , Position
  , Compression(..)
  , Size

  -- * Exceptions
  , SomeLZMAException(..)
  , CompressException(..)
  , DecompressException(..)
  , DecodeException(..)

  -- * Utils
  , hasMagicBytes
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
import Foreign.Var
import Pipes hiding (next, void)
import Pipes.Core
import Pipes.Safe ()
import qualified Control.Monad.ST as S
import qualified Control.Monad.ST.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
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

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
data CompressParams = CompressParams
  { compressPreset :: !C.Preset
  -- ^
  , compressIntegrityCheck :: !C.Check
  -- ^
  , compressBufferSize :: !Int
  -- ^
  , compressMemoryLimit :: !Word64
  -- ^
  }

-- | The default set of parameters for compression. This is typically used with
-- the 'compressWith' function with specific paramters opverridden.
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams
  { compressPreset = C.defaultPreset
  , compressIntegrityCheck = C.CheckCrc64
  , compressBufferSize = defaultCompressBufferSize
  , compressMemoryLimit = maxBound -- No limit
  }

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

-- | The default set of parameters for decompression. This is typically used
-- with the 'decompressWith' function with specific parameters overridden.
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams
  { decompressBufferSize = defaultDecompressBufferSize
  , decompressMemoryLimit = maxBound -- No limit
  }

defaultCompressBufferSize :: Int
defaultCompressBufferSize = 16 * 1024 - L.chunkOverhead

defaultDecompressBufferSize :: Int
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead

-----------------------------------------------------------
-- Compression

-- | The unfolding of the compression process, where you provide a sequence of
-- uncompressed data chunks as input and receive a sequence of compressed data
-- chunks as output. The process is incremental, in that the demand for input
-- and provision of output are interleaved.
type CompressStream = Pipe S.ByteString S.ByteString

-- |
data CompressException = CompressError
  Stream.ErrorCode
  --- ^
  String
  --- ^
  deriving (Eq, Show, Typeable)

handleCompRet
  :: (MonadTrans t, Monad (t m), MonadThrow m)
  => String -- ^ Description of an error if exists
  -> t m C.Ret
  -> t m ()
handleCompRet reason m = do
  ret <- m
  case ret of
    C.Error code -> lift $ throwCompressError code reason
    _ -> return ()

throwCompressError
  :: MonadThrow m
  => C.ErrorCode
  -> String -- ^ Description of the error
  -> m a
throwCompressError = (throwM .) . CompressError

-- |
instance Exception CompressException where
  toException = Stream.lzmaExceptionToException
  fromException = Stream.lzmaExceptionFromException

compress :: CompressParams -> L.ByteString -> L.ByteString
compress = streamToLBS . compressStream

compressST :: CompressParams -> CompressStream (L.ST s) ()
compressST = streamToST . compressStream

compressIO :: CompressParams -> CompressStream IO ()
compressIO = streamToIO . compressStream

compressStream :: CompressParams -> CompressStream Stream ()
compressStream params = do
  handleCompRet "Failed to initialize a stream encoder" $
    lift $ Stream.easyEncoder
      (compressPreset params)
      (compressIntegrityCheck params)
  loop
  where
    loop = fillBuffers (compressBufferSize params) () >>= drainBuffers
    drainBuffers isLastChunk = do
      lift $ assertBuffers isLastChunk

      res <- lift $ Stream.code $ if isLastChunk
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
        Stream.Error code -> do
          void $ finalizeStream 0
          lift $ throwCompressError code "The stream encoder failed."

-----------------------------------------------------------
-- Decompression

-- | The unfolding of the decompression process, where you provide a sequence
-- of compressed data chunks as input and receive a sequence of uncompressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
type DecompressStream = Pipe S.ByteString S.ByteString

handleDecompRet
  :: (MonadTrans t, Monad (t m), MonadThrow m)
  => String -- ^ Description of an error if exists
  -> t m C.Ret
  -> t m ()
handleDecompRet reason m = do
  ret <- m
  case ret of
    C.Error code -> lift $ throwDecompressError code reason
    _ -> return ()

throwDecompressError
  :: MonadThrow m
  => C.ErrorCode
  -> String -- ^ Description of the error
  -> m a
throwDecompressError = (throwM .) . DecompressError

-- | The possible error cases when decompressing a stream.
data DecompressException = DecompressError
  Stream.ErrorCode
  --- ^ Error code from liblzma
  String
  --- ^ Description of the error
  deriving (Eq, Show, Typeable)

instance Exception DecompressException where
  toException = Stream.lzmaExceptionToException
  fromException = Stream.lzmaExceptionFromException

decompress :: DecompressParams -> L.ByteString -> L.ByteString
decompress = streamToLBS . decompressStream

decompressST :: DecompressParams -> DecompressStream (L.ST s) ()
decompressST = streamToST . decompressStream

decompressIO :: DecompressParams -> DecompressStream IO ()
decompressIO = streamToIO . decompressStream

decompressStream :: DecompressParams -> DecompressStream Stream ()
decompressStream params = do
  handleDecompRet "Failed to initialize a stream decoder" $
    lift $ Stream.autoDecoder (decompressMemoryLimit params) mempty
  loop
  where
    loop = fillBuffers (decompressBufferSize params) () >>= drainBuffers
    drainBuffers isLastChunk = do
      lift $ assertBuffers isLastChunk

      res <- lift $ Stream.code $ if isLastChunk
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
        Stream.Error code -> do
          void $ finalizeStream 0
          lift $ throwDecompressError code "The stream decoder failed."

-----------------------------------------------------------

-- | Convert a (de)compression stream into a lazy 'L.ByteString' transformer.
streamToLBS
  :: Proxy x' S.ByteString () S.ByteString Stream a
  -> L.ByteString -> L.ByteString
streamToLBS stream input = L.runST $ do
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
      -- a lazy bytestring shouldn't contain empty chunks
      return $! if S.length outChunk > 0
        then L.Chunk outChunk outChunks
        else outChunks
    go (P.M m) state inChunks = do
      (next, state') <- L.strictToLazyST $ Stream.runStream m state
      go next state' inChunks
    go (P.Pure _) _ !_inChunks = return L.Empty

streamToST
  :: Proxy x' x y' y Stream a
  -> Proxy x' x y' y (L.ST s) a
streamToST stream = do
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

streamToIO
  :: Proxy x' x y' y Stream a
  -> Proxy x' x y' y IO a
streamToIO stream = do
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
  streamToIO . seekableDecompressStream params index

seekableDecompressStream
  :: DecompressParams
  -> C.Index
  -- ^ Index for the stream
  -> ReadRequest 'Uncompressed
  -- ^ Initial request
  -> SeekableDecompressStream Stream ()
seekableDecompressStream params index req0 = do
  iter <- liftIO $ C.indexIterInit index
  decodeLoop iter req0
  where
    decodeLoop iter req = do
      found <- locateBlock iter req
      when found $ do
        req' <- decodeBlock iter req
        decodeLoop iter req'

    -- | Decode a new block
    decodeBlock
      :: C.IndexIter
      -> ReadRequest 'Uncompressed
      -> SeekableDecompressStream Stream (ReadRequest 'Uncompressed)
    decodeBlock iter req = do
      lift Stream.flushBuffers
#if DEBUG
      liftIO $ C.dumpIndexIter iter
      traceM $ "decodeBlock " ++ show iter ++ " " ++ show req
#endif
      blockCount <- liftIO $ get $ C.indexIterStreamBlockCount iter
      compressedFileOffset <-
        liftIO $ get $ C.indexIterBlockCompressedFileOffset iter
      uncompressedFileOffset <-
        liftIO $ get $ C.indexIterBlockUncompressedFileOffset iter
      blockNumberInStream <-
        liftIO $ get $ C.indexIterBlockNumberInStream iter
      let
        blockPos :: Position 'Compressed
        blockPos = fromIntegral compressedFileOffset
        blockUncompPos :: Position 'Uncompressed
        blockUncompPos = fromIntegral uncompressedFileOffset
      -- Check if the block number doesn't exceed the total block count.
      assert (blockNumberInStream <= blockCount) $ return ()
      isLastChunk <- fillBuffers (decompressBufferSize params) (PRead blockPos)
      block <- liftIO C.newBlock
      filters <- liftIO C.newFiltersMaxLength
      handleDecompRet "Failed to initialize a block decoder" $
        lift $ Stream.blockDecoder iter block filters
#if DEBUG
      lift $ Stream.dump "decodeBlock"
#endif
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
      :: C.IndexIter
      -> ReadRequest 'Compressed
      -> Int -- ^ Offset from the beginning of the block to the target position
      -> SeekableDecompressStream Stream (ReadRequest 'Uncompressed)
    fillBuffers' iter req skipBytes = do
      isLastChunk <- fillBuffers (decompressBufferSize params) req
      drainBuffers iter isLastChunk skipBytes

    drainBuffers
      :: C.IndexIter
      -> Bool -- ^ Last chunk or not
      -> Int -- ^ Offset from the beginning of the block to the target position
      -> SeekableDecompressStream Stream (ReadRequest 'Uncompressed)
    drainBuffers iter isLastChunk skipBytes = do
      lift $ assertBuffers isLastChunk

      ret <- lift $ Stream.code $ if isLastChunk
        then Stream.Finish
        else Stream.Run
#if DEBUG
      traceM $ "code -> " ++ show ret
#endif
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

        Stream.Error code -> do
          void $ finalizeStream skipBytes
          lift $ throwDecompressError code "Failed to decode a block"

-- | If the 'ReadRequest' has a position, find the block which contains the
-- position. If it doesn't have a position, find the next non-empty block.
--
-- This function return true when the block is found.
locateBlock
  :: C.IndexIter
  -> ReadRequest 'Uncompressed
  -> SeekableDecompressStream Stream Bool
locateBlock iter req = not <$> liftIO act
  where
    act = case req of
      -- If the request has a position, decode the block which contains the
      -- position.
      PRead pos -> C.indexIterLocate iter (fromIntegral pos)
      -- If the request doesn't have a position, continue reading bytes from
      -- the current position.
      Read -> C.indexIterNext iter C.IndexIterNonEmptyBlockMode

fillBuffers
  :: Int -- ^ Buffer size
  -> r -- ^ Request
  -> Proxy r S.ByteString y' y Stream Bool
fillBuffers bufferSize req = do
#ifdef DEBUG
  lift $ Stream.consistencyCheck
#endif
  fillOutputBuffer
  fillInputBuffer
  where
    fillOutputBuffer = lift $ do
      outputBufferFull <- Stream.isOutputBufferFull
      when outputBufferFull $ do
        outFPtr <- liftIO $ S.mallocByteString bufferSize
        Stream.pushOutputBuffer outFPtr 0 bufferSize

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

decodeIndex :: Handle -> IO (C.Index, C.VLI)
decodeIndex h = do
  size <- hFileSize h
  runDecodeStream h $ decodeIndexIO (fromIntegral size)

decodeIndexIO :: Size -> DecodeStream IO (C.Index, C.VLI)
decodeIndexIO size = bracket acquire release $ uncurry $
  indexDecodingToIO
    (decodeIndexStream (fromIntegral size))
    ID.newIndexDecoderState
  where
    acquire = liftIO $ (,) <$> C.mallocStreamFlags <*> C.mallocStreamFlags
    release (header, footer) = liftIO $ do
      C.freeStreamFlags header
      C.freeStreamFlags footer

runDecodeStream
  :: (MonadIO m, MonadThrow m)
  => Handle
  -> DecodeStream m a
  -> m a
runDecodeStream h = runEffect . loop
  where
    loop (P.Request seekRequest next) = do
      case seekRequest of
        PRead (fromIntegral -> pos) -> do
          r <- liftIO $ try $ hSeek h AbsoluteSeek pos
          case r of
            Left e -> lift $ throwM (e :: IOException)
            Right () -> return ()
        Read -> return ()
      chunk <- liftIO $ S.hGetSome h L.defaultChunkSize
      loop (next chunk)
    loop (P.Respond _ next) = loop (next ())
    loop (P.M m) = lift m >>= loop
    loop (P.Pure a) = return a

-- | Seek to an absolute position and ask for an input with given size.
-- Note that this is inefficient when you ask for a large amount of bytes.
pread
  :: Monad m
  => Position 'Compressed
  -> Size
  -- ^ Input size
  -> DecodeStream m S.ByteString
pread pos size = do
  builder <- loop size (PRead pos) mempty
  return $! L.toStrict $ B.toLazyByteString builder
  where
    loop nbytes req chunks
      | nbytes <= 0 = return chunks
      | otherwise = do
        chunk <- request req
        let chunks' = chunks <> B.byteString (S.take nbytes chunk)
        loop (nbytes - S.length chunk) Read chunks'

-- | Decode things from compressed stream.
type DecodeStream = Client (ReadRequest 'Compressed) S.ByteString

-- | Seek operation failure in downstream.
data DecodeException = DecodeError
  C.ErrorCode
  --- ^ Error code from liblzma
  String
  --- ^ Description of the error
  deriving (Show, Typeable)

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
  index <- decodeIndex1
  loop index
  padding <- lift ID.getStreamPadding
  return (index, padding)
  where
    loop :: C.Index -> DecodeStream IndexDecoder ()
    loop index = do
      pos <- lift ID.getPosition
      when (pos > 0) $ do
        index' <- decodeIndex1
        handleDecompRet "Failed to concatenate indicies." $
          liftIO $ C.indexCat index index'
        loop index

-- | Parse an index
decodeIndex1 :: DecodeStream IndexDecoder C.Index
decodeIndex1 = do
  padding <- parseStreamFooter
  index <- parseIndex 8192 -- FIXME: Set appropreate size
  parseStreamHeader index
  checkIntegrity index
  handleDecompRet "Failed to set stream padding" $
    liftIO $ C.indexStreamPadding index padding
  lift $ ID.modifyStreamPadding' (+ padding)
  return index

-- | Skip stream padding if exists then parse a stream footer. It returns the
-- the length of the stream padding.
parseStreamFooter :: DecodeStream IndexDecoder C.VLI
parseStreamFooter = loop 0
  where
    loop padding = do
      endPos <- lift ID.getPosition
      when (endPos < 2 * headerSize) $
        lift $ throwM $ DecodeError C.DataError
          "This file is too small to be a valid .xz file."

      chunk@(S.PS inFPtr inOffset inLength) <-
        pread (endPos - footerSize) footerSize
      assert (inOffset == 0 && inLength == footerSize) $ return ()

      if containsStreamPadding chunk
        then do
          padding' <- lift $ skipStreamPadding chunk
          loop $! padding + padding'
        else do
          lift $ ID.setPosition $ endPos - footerSize
          footer <- lift ID.getStreamFooter
          handleDecompRet "Failed to decode a stream footer." $
            liftIO $ withForeignPtr inFPtr $ C.streamFooterDecode footer
          version <- liftIO $ get $ C.streamFlagsVersion footer
          unless (version ==  0) $
            lift $ throwM $ DecodeError C.OptionsError
              "The stream footer specifies something that we don't support."
          liftIO $ touchForeignPtr inFPtr
          return padding

skipStreamPadding :: S.ByteString -> IndexDecoder C.VLI
skipStreamPadding chunk =
  assert (paddingLength `mod` 4 == 0) $ do
    ID.modifyPosition' (subtract $ fromIntegral paddingLength)
    return $! fromIntegral paddingLength
  where
    (_, S.length -> paddingLength) = S.spanEnd (== 0) chunk

containsStreamPadding :: S.ByteString -> Bool
containsStreamPadding = S.all (== 0) . S.take 4 . S.drop 8

getIndexSize :: IndexDecoder C.VLI
getIndexSize = do
  footer <- ID.getStreamFooter
  liftIO $ get $ C.streamFlagsBackwardSize footer

-- | Decode a stream index.
parseIndex
  :: C.VLI -- ^ Buffer size
  -> DecodeStream IndexDecoder C.Index
parseIndex bufSize = do
  indexSize <- lift getIndexSize
  -- Set posision to the beginning of the index.
  lift $ ID.modifyPosition' $ subtract $ fromIntegral indexSize
  stream <- liftIO C.newStream
  (ret, indexFPtr) <- liftIO $ C.indexDecoder stream maxBound -- FIXME: Set proper value
  unless (ret == C.Ok) $ lift $ throwM $ DecodeError C.ProgError
    "Failed to initialize an index decoder."

  loop stream indexSize

  liftIO $ C.peekIndexFPtr indexFPtr
  where
    loop stream indexSize = do
      let inAvail :: Integral a => a
          inAvail = fromIntegral $ min bufSize indexSize
      liftIO $ C.streamAvailIn stream $=! inAvail
      chunk <- do
        pos <- lift ID.getPosition
        pread pos inAvail
      lift $ ID.modifyPosition' (+ inAvail)
      let indexSize' = indexSize - inAvail
      ret <- liftIO $ withByteString chunk $ \inPtr -> do
        C.streamNextIn stream $= inPtr
        C.code stream C.Run
      case ret of
        C.Ok -> loop stream indexSize'
        C.Error C.BufError ->
          lift $ throwM $ DecodeError C.DataError $
            "The index decoder has liked more input than what the index " ++
            "should be according to stream footer."
        C.Error code ->
          lift $ throwM $ DecodeError code "The index decoder faild."
        C.StreamEnd -> do
          inAvail' <- liftIO $ get $ C.streamAvailIn stream
          unless (indexSize' == 0 && inAvail' == 0) $
            lift $ throwM $ DecodeError C.DataError $
              "The index decoder didn't consume as much input as indicated " ++
              "by the backward size field."

-- | Decode the stream header and check that its stream flags match the stream
-- footer.
parseStreamHeader
  :: C.Index
  -> DecodeStream IndexDecoder ()
parseStreamHeader index = do
  indexSize <- lift getIndexSize
  lift $ ID.modifyPosition' (subtract $ fromIntegral indexSize + headerSize)
  blocksSize <- liftIO $ C.indexTotalSize index
  lift $ ID.modifyPosition' (subtract $ fromIntegral blocksSize)
  chunk <- do
    pos <- lift ID.getPosition
    pread pos headerSize
  header <- lift ID.getStreamHeader
  handleDecompRet "Failed to decode a stream header." $
    liftIO $ withByteString chunk $ C.streamHeaderDecode header

checkIntegrity
  :: C.Index
  -> DecodeStream IndexDecoder ()
checkIntegrity index = do
  header <- lift ID.getStreamHeader
  footer <- lift ID.getStreamFooter
  handleDecompRet "The stream header and the footer didn't agree." $
    liftIO $ C.streamFlagsCompare header footer
  handleDecompRet "Failed to set the footer to the index." $
    liftIO $ C.indexStreamFlags index footer
  padding <- lift ID.getStreamPadding
  handleDecompRet "Failed to set stream padding to the index." $
    liftIO $ C.indexStreamPadding index padding

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

-----------------------------------------------------------

hasMagicBytes :: Monad m => DecodeStream m Bool
hasMagicBytes = do
  chunk <- pread beginning 6
  return $! chunk == "\xfd\&7zXZ\x00" -- \& is an empty string
  where
    beginning :: Position 'Compressed
    beginning = 0

-----------------------------------------------------------

withByteString :: S.ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteString (S.PS fptr off _len) f =
    withForeignPtr fptr $ \ptr ->
        f (advancePtr ptr off)
