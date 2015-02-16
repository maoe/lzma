{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Codec.Compression.LZMA.Internal
  ( -- * Decompression
    DecompressParams(..)
  , defaultDecompressParams

  -- ** Lazy 'L.ByteString's
  , decompress

  -- ** Incremental processing
  -- *** Decompression
  , DecompressStream, DecompressError(..)
  , decompressST
  , decompressIO
  -- , decompressStream
  -- ** Random access
  , IndexedDecompressStream
  , indexedDecompressIO
  ) where
import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Word
import Foreign hiding (void)

import Control.Monad.Catch
import Pipes hiding (next, void)
import Pipes.Core
import qualified Control.Monad.ST as S
import qualified Control.Monad.ST.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Pipes.Internal as P

import Codec.Compression.LZMA.Internal.Index
import Codec.Compression.LZMA.Internal.Stream (Stream)
import qualified Codec.Compression.LZMA.Internal.Stream as Stream
import qualified Codec.Compression.LZMA.Internal.C as C

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
  }

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
  unless (ret == C.Ok) $ throwStreamError $ DecompressError C.ProgError

throwStreamError
  :: (MonadTrans t, MonadThrow m, Exception e)
  => e -- ^ Exception to throw
  -> t m a
throwStreamError = lift . throwM . Stream.SomeStreamException

-- | The possible error cases when decompressing a stream.
data DecompressError = DecompressError Stream.ErrorCode
  deriving (Eq, Show, Typeable)

instance Exception DecompressError where
  toException = Stream.streamExceptionToException
  fromException = Stream.streamExceptionFromException

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
        Stream.Error code -> do
          void $ finalizeStream 0
          throwStreamError $ DecompressError code

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

type IndexedDecompressStream = Proxy
  (ReadRequest 'Compressed) S.ByteString
  (ReadRequest 'Uncompressed) S.ByteString

indexedDecompressIO
  :: DecompressParams
  -> Index
  -> ReadRequest 'Uncompressed
  -> IndexedDecompressStream IO ()
indexedDecompressIO params index =
  decompressStreamToIO . indexedDecompressStream params index

indexedDecompressStream
  :: DecompressParams
  -> Index
  -> ReadRequest 'Uncompressed
  -> IndexedDecompressStream Stream ()
indexedDecompressStream params index req0 = do
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
      -> IndexedDecompressStream Stream (ReadRequest 'Uncompressed)
    decodeBlock iter req = do
      lift Stream.flushBuffers
      indexIter@IndexIter {..} <- liftIO $ withForeignPtr iter peek
      let
        IndexIterStream {indexIterStreamBlockCount} = indexIterStream
        IndexIterBlock {..} = indexIterBlock
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
      -> IndexedDecompressStream Stream (ReadRequest 'Uncompressed)
    fillBuffers' iter req skipBytes = do
      isLastChunk <- fillBuffers params req
      drainBuffers iter isLastChunk skipBytes

    drainBuffers
      :: C.IndexIterPtr
      -> Bool -- ^ Last chunk or not
      -> Int -- ^ Offset from the beginning of the block to the target position
      -> IndexedDecompressStream Stream (ReadRequest 'Uncompressed)
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

        Stream.Error code -> do
          void $ finalizeStream skipBytes
          throwStreamError $ DecompressError code

-- | If the 'ReadRequest' has a position, find the block which contains the
-- position. If it doesn't have a position, find the next non-empty block.
--
-- This function return true when the block is found.
locateBlock
  :: C.IndexIterPtr
  -> ReadRequest 'Uncompressed
  -> IndexedDecompressStream Stream Bool
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
        outFPtr <- Stream.unsafeLiftIO $ S.mallocByteString decompressBufferSize
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
