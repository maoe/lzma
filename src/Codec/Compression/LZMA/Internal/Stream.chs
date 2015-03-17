{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Codec.Compression.LZMA.Internal.Stream
  ( Stream
  , runStream
  , State
  , newState

  -- ** High level API
  , easyEncoder
  , autoDecoder
  , code
  , end

  -- *** Block
  , blockDecoder

  -- ** Basic types
  , C.Ret(..), C.ErrorCode(..)
  , C.Action(..)

  -- * Buffer manipulation
  , flushBuffers
  -- ** Input buffer
  , pushInputBuffer
  , isInputBufferEmpty
  , remainingInputBuffer
  -- ** Output buffer
  , pushOutputBuffer
  , popOutputBuffer
  , outputBufferBytesAvailable
  , isOutputBufferFull
  , outputBufferSpaceRemaining

  -- * Exceptions
  , SomeLZMAException(..)
  , lzmaExceptionToException
  , lzmaExceptionFromException
  , LZMAException(..)

#ifdef DEBUG
  -- * Debugging tools
  , debug
  , dump
  , consistencyCheck
#endif
  ) where
import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Foreign
import Foreign.C
import qualified Foreign.ForeignPtr.Unsafe as Unsafe (unsafeForeignPtrToPtr)

import Control.Monad.Catch
import Control.Monad.ST
import Control.Monad.Trans (MonadIO(..))
import Data.Vector.Storable.Mutable (IOVector)
import Foreign.Var
import qualified Control.Monad.ST.Unsafe as Unsafe (unsafeIOToST)
import qualified Data.ByteString.Internal as S (nullForeignPtr)

{# import Codec.Compression.LZMA.Internal.C #} ()
import Codec.Compression.LZMA.Internal.Types
import qualified Codec.Compression.LZMA.Internal.C as C

#ifdef DEBUG
import Debug.Trace
import System.IO (hPrint, hPutStrLn, stderr)
#endif

#include <lzma.h>

{# context lib="lzma" prefix="lzma" #}

-- | The Stream monad, which maintains pointers to the underlying stream state,
-- the input buffer and the output buffer.
newtype Stream a = Stream
  { unStream
      :: C.Stream
      -> ForeignPtr Word8 -- Input buffer
      -> ForeignPtr Word8 -- Output buffer
      -> Int -- Offset of the output
      -> Int -- Length of the output
      -> IO (ForeignPtr Word8, ForeignPtr Word8, Int, Int, a)
  }

instance Functor Stream where
  fmap = liftM

instance Applicative Stream where
  pure = return
  (<*>) = ap

instance Monad Stream where
  return a = Stream $ \_ inBuf outBuf offset len ->
    return (inBuf, outBuf, offset, len, a)
  Stream m >>= k = Stream $ \stream inBuf outBuf outOffset outLength -> do
    (inBuf', outBuf', outOffset', outLength', a) <-
      m stream inBuf outBuf outOffset outLength
    unStream (k a) stream inBuf' outBuf' outOffset' outLength'

instance MonadIO Stream where
  liftIO = unsafeLiftIO

unsafeLiftIO :: IO a -> Stream a
unsafeLiftIO m = Stream $ \_stream inBuf outBuf outOffset outLength -> do
  a <- m
  return (inBuf, outBuf, outOffset, outLength, a)

instance MonadThrow Stream where
  throwM = unsafeLiftIO . throwM

instance MonadCatch Stream where
  catch (Stream m) handler =
    Stream $ \stream inBuf outBuf outOffset outLength ->
      m stream inBuf outBuf outOffset outLength
        `catch` \e ->
          unStream (handler e) stream inBuf outBuf outOffset outLength

instance MonadMask Stream where
  mask f = Stream $ \stream inBuf outBuf outOffset outLength ->
    mask $ \restore ->
      unStream (f $ g restore) stream inBuf outBuf outOffset outLength
    where
      g :: (forall b. IO b -> IO b) -> Stream a -> Stream a
      g restore (Stream m) = Stream $ \stream inBuf outBuf outOffset outLength ->
        restore $ m stream inBuf outBuf outOffset outLength

  uninterruptibleMask f = Stream $ \stream inBuf outBuf outOffset outLength ->
    uninterruptibleMask $ \restore ->
      unStream (f $ g restore) stream inBuf outBuf outOffset outLength
    where
      g :: (forall b. IO b -> IO b) -> Stream a -> Stream a
      g restore (Stream m) = Stream $ \stream inBuf outBuf outOffset outLength ->
        restore $ m stream inBuf outBuf outOffset outLength

-- | Add a new input buffer.
pushInputBuffer
  :: ForeignPtr Word8
  -- ^ Pointer to the payload of a buffer
  -> Int -- ^ Offset for the buffer
  -> Int -- ^ Length of the buffer
  -> Stream ()
pushInputBuffer inBuf inOffset inLen = do
  inAvail <- getInAvail
  assert (inAvail == 0) $ return ()

  oldInBuf <- getInBuf
  liftIO $ touchForeignPtr oldInBuf

  setInBuf inBuf
  setInAvail inLen
  setInNext $ Unsafe.unsafeForeignPtrToPtr inBuf `plusPtr` inOffset

isInputBufferEmpty :: Stream Bool
isInputBufferEmpty = (0 ==) <$> getInAvail

remainingInputBuffer :: Stream (ForeignPtr Word8, Int, Int)
remainingInputBuffer = do
  inBuf <- getInBuf
  inNext <- getInNext
  inAvail <- getInAvail

  assert (inAvail > 0) $ return ()

  return (inBuf, inNext `minusPtr` Unsafe.unsafeForeignPtrToPtr inBuf, inAvail)

pushOutputBuffer
  :: ForeignPtr Word8 -- ^
  -> Int -- ^
  -> Int -- ^
  -> Stream ()
pushOutputBuffer outBuf outOffset outLen = do
  outAvail <- getOutAvail
  assert (outAvail == 0) $ return ()

  oldOutBuf <- getOutBuf
  liftIO $ touchForeignPtr oldOutBuf

  setOutBuf outBuf
  setOutFree outLen
  setOutNext $ Unsafe.unsafeForeignPtrToPtr outBuf `plusPtr` outOffset

  setOutOffset outOffset
  setOutAvail 0

-- | Get the part of the output buffer that is currently full (might be 0, use
-- 'outputBufferBytesAvailable' to check). This may leave some space remaining
-- in the buffer, use 'outputBufferSpaceRemaining' to check.
popOutputBuffer :: Stream (ForeignPtr Word8, Int, Int)
popOutputBuffer = do
  outBuf <- getOutBuf
  outOffset <- getOutOffset
  outAvail <- getOutAvail

  assert (outAvail > 0) $ return ()

  setOutOffset $ outOffset + outAvail
  setOutAvail 0

  return (outBuf, outOffset, outAvail)

outputBufferBytesAvailable :: Stream Int
outputBufferBytesAvailable = getOutAvail

outputBufferSpaceRemaining :: Stream Int
outputBufferSpaceRemaining = getOutFree

isOutputBufferFull :: Stream Bool
isOutputBufferFull = (0 ==) <$> outputBufferSpaceRemaining

flushBuffers :: Stream ()
flushBuffers = do
  setInNext nullPtr
  setInAvail 0
  setOutNext nullPtr
  setOutFree 0
  setOutAvail 0
  setOutOffset 0
  nullFPtr <- liftIO $ newForeignPtr_ nullPtr
  setOutBuf nullFPtr

------------------------------------------------------------
-- Stream monad

-- | Opaque data type to hold the underlying state in the 'Stream' monad.
data State s = State
  { stateStream :: !C.Stream
  , stateInBuf :: !(ForeignPtr Word8)
  , stateOutBuf :: !(ForeignPtr Word8)
  , stateOutOffset :: !Int
  , stateOutLength :: !Int
  }

newState :: ST s (State s)
newState = Unsafe.unsafeIOToST $ do
  stream <- C.newStream
  return $ State stream S.nullForeignPtr S.nullForeignPtr 0 0

runStream :: Stream a -> State s -> ST s (a, State s)
runStream (Stream m) (State {..}) = Unsafe.unsafeIOToST $ do
  (inBuf, outBuf, outOff, outLen, a) <-
    m stateStream stateInBuf stateOutBuf stateOutOffset stateOutLength
  return (a, State stateStream inBuf outBuf outOff outLen)

getStream :: Stream C.Stream
getStream = Stream $ \stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, stream)

getInBuf :: Stream (ForeignPtr Word8)
getInBuf = Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, inBuf)

setInBuf :: ForeignPtr Word8 -> Stream ()
setInBuf inBuf = Stream $ \_stream _inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, ())

getOutBuf :: Stream (ForeignPtr Word8)
getOutBuf = Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, outBuf)

setOutBuf :: ForeignPtr Word8 -> Stream ()
setOutBuf outBuf = Stream $ \_stream inBuf _outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, ())

getOutOffset :: Stream Int
getOutOffset = Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, outOffset)

setOutOffset :: Int -> Stream ()
setOutOffset outOffset = Stream $ \_stream inBuf outBuf _ outLength ->
  return (inBuf, outBuf, outOffset, outLength, ())

getOutAvail :: Stream Int
getOutAvail = Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, outLength)

setOutAvail :: Int -> Stream ()
setOutAvail outLength = Stream $ \_stream inBuf outBuf outOffset _ ->
  return (inBuf, outBuf, outOffset, outLength, ())

------------------------------------------------------------

withStreamPtr :: (Ptr C.Stream -> IO a) -> Stream a
withStreamPtr f = do
  stream <- getStream
  liftIO $ C.withStream stream f

getInAvail :: Stream Int
getInAvail = do
  fromIntegral <$> withStreamPtr {# get lzma_stream.avail_in #}

setInAvail :: Int -> Stream ()
setInAvail n = withStreamPtr $ \p ->
  {# set lzma_stream.avail_in #} p (fromIntegral n)

getInNext :: Stream (Ptr Word8)
getInNext = castPtr <$> withStreamPtr {# get lzma_stream.next_in #}

setInNext :: Ptr Word8 -> Stream ()
setInNext p' = withStreamPtr $ \p ->
  {# set lzma_stream.next_in #} p (castPtr p')

getOutFree :: Stream Int
getOutFree =
  fromIntegral <$> withStreamPtr {# get lzma_stream.avail_out #}

setOutFree :: Int -> Stream ()
setOutFree n = withStreamPtr $ \p ->
  {# set lzma_stream.avail_out #} p (fromIntegral n)

#if DEBUG
getOutNext :: Stream (Ptr Word8)
getOutNext = castPtr <$> withStreamPtr {# get lzma_stream.next_out #}
#endif

setOutNext :: Ptr Word8 -> Stream ()
setOutNext p' = withStreamPtr $ \p ->
  {# set lzma_stream.next_out #} p (castPtr p')

-- | Initialize .xz Stream encoder using a preset number.
--
-- This function is intended for those who just want to use the basic features
-- if liblzma (that is, most developers out there).
easyEncoder
  :: C.Preset
  -- ^ Compression preset to use.
  --
  -- A preset consist of level number and zero or
  -- more flags. Usually flags aren't used, so preset is simply a number [0, 9]
  -- which match the options -0 ... -9 of the xz command line tool. Additional
  -- flags can be be set using '<>' from @Monoid@ with the preset level number,
  -- e.g. @'customPreset' 6 <> 'extremePreset'@.
  -> C.Check
  -- ^ Integrity check type to use. See 'Check' for available checks. The xz
  -- command line tool defaults to 'CheckCrc64', which is a good choice if you
  -- are unsure. 'CheckCrc32' is good too as long as the uncompressed file is
  -- not many gigabytes.
  -> Stream C.Ret
easyEncoder preset check = do
  s <- getStream
  liftIO $ C.easyEncoder s preset check

-- | Decode .xz Streams and .lzma files with autodetection.

-- This decoder autodetects between the .xz and .lzma file formats, and calls
-- 'lzma_stream_decoder' or 'lzma_alone_decoder' once the type of the input
-- file has been detected.
autoDecoder
  :: Word64
  -- ^ Memory usage limit as bytes. Use 'maxBound' to effectively disable the
  -- limiter.
  -> C.Flags
  -- ^ @<>@ of flags, or @mempty@ for no flags.
  -> Stream C.Ret
autoDecoder memLimit flags = do
  stream <- getStream
  liftIO $ C.autoDecoder stream memLimit flags

-- | Encode or decode data.
--
-- Once the 'Stream' has been successfully initialized, the actual encoding
-- or decoding is done using this function.
code :: C.Action -> Stream C.Ret
code action = do
  outFree <- getOutFree
  assert (outFree > 0) $ return ()

#ifdef DEBUG
  outNext <- getOutNext
  assert (outNext /= nullPtr) $ return ()
#endif

  stream <- getStream
  ret <- liftIO $ C.code stream action

  outFree' <- getOutFree
  let outExtra = outFree - outFree'
  outAvail <- getOutAvail
  setOutAvail $ outAvail + outExtra
  return ret

-- | Free memory allocated for the coder data structures.
end :: Stream ()
end = getStream >>= liftIO . C.end

blockDecoder :: C.IndexIter -> C.Block -> IOVector C.Filter -> Stream C.Ret
blockDecoder iter block filters = do
  streamFlags <- liftIO $ get $ C.indexIterStreamFlags iter
  unpaddedSize <- liftIO $ get $ C.indexIterBlockUnpaddedSize iter

  inNext <- getInNext

  liftIO $ do
    C.blockVersion block $= 0
    C.blockFilters block $= filters

    check <- get $ C.streamFlagsCheck streamFlags
    C.blockCheck block $= check

    firstByte <- peek inNext
    C.blockHeaderSize block $= C.blockHeaderSizeDecode firstByte

  inAvail <- getInAvail
  handleRet $ liftIO $ C.blockHeaderDecode block inNext

  handleRet $
    liftIO $ C.blockCompressedSize block unpaddedSize

  headerSize <- liftIO $ get $ C.blockHeaderSize block

  setInNext $ inNext `advancePtr` fromIntegral headerSize
  setInAvail $ inAvail - fromIntegral headerSize

  stream <- getStream
  liftIO $ C.blockDecoder stream block
  where
    handleRet m = do
      ret <- m
      case ret of
        C.Error errorCode -> throwM (LZMAErrorCode errorCode)
        _ -> return ()

------------------------------------------------------------
-- Debugging tools

#if DEBUG

debug :: Show a => a -> Stream ()
debug = liftIO . hPrint stderr

dump :: String -> Stream ()
dump label = do
  inNext  <- getInNext
  inAvail <- getInAvail

  outNext <- getOutNext
  outFree <- getOutFree
  outAvail <- getOutAvail
  outOffset <- getOutOffset

  inTotal <- withStreamPtr {# get lzma_stream.total_in #}
  outTotal <- withStreamPtr {# get lzma_stream.total_out #}

  liftIO $ hPutStrLn stderr $
    label ++ ": " ++
    "Stream {\n" ++
    "  inNext    = " ++ show inNext    ++ ",\n" ++
    "  inAvail   = " ++ show inAvail   ++ ",\n" ++
    "\n" ++
    "  outNext   = " ++ show outNext   ++ ",\n" ++
    "  outFree   = " ++ show outFree   ++ ",\n" ++
    "  outAvail  = " ++ show outAvail  ++ ",\n" ++
    "  outOffset = " ++ show outOffset ++ ",\n" ++
    "\n" ++
    "  inTotal   = " ++ show inTotal   ++ ",\n" ++
    "  outTotal  = " ++ show outTotal  ++ "\n" ++
    "}"

consistencyCheck :: Stream ()
consistencyCheck = do
  outBuf <- getOutBuf
  outOffset <- getOutOffset
  outAvail <- getOutAvail
  outNext <- getOutNext

  let outBufPtr = Unsafe.unsafeForeignPtrToPtr outBuf

  assert (outBufPtr `plusPtr` (outOffset + outAvail) == outNext) $ return ()

#endif
