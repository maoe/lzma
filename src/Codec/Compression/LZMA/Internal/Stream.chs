{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Codec.Compression.LZMA.Internal.Stream
  ( M.Stream
  , runStream
  , State
  , newState

  -- ** High level API
  , compress
  , decompress

  -- ***
  , easyEncoder
  , autoDecoder
  , code
  , end

  -- *** Block
  , blockDecoder

  -- ** Basic types
  , Ret(..), ErrorCode(..)
  , Action(..)

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
import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Control.Monad.Catch
import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.Trans (liftIO)
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.ByteString.Internal as S (nullForeignPtr)

{# import Codec.Compression.LZMA.Internal.C #}
import Codec.Compression.LZMA.Internal.Types
import qualified Codec.Compression.LZMA.Internal.Stream.Monad as M

#ifdef DEBUG
import Debug.Trace
import System.IO (hPrint, hPutStrLn, stderr)
#endif

#include <lzma.h>

{# context lib="lzma" prefix="lzma" #}

-- | Add a new input buffer.
pushInputBuffer
  :: ForeignPtr Word8
  -- ^ Pointer to the payload of a buffer
  -> Int -- ^ Offset for the buffer
  -> Int -- ^ Length of the buffer
  -> M.Stream ()
pushInputBuffer inBuf inOffset inLen = do
  inAvail <- getInAvail
  assert (inAvail == 0) $ return ()

  oldInBuf <- getInBuf
  liftIO $ touchForeignPtr oldInBuf

  setInBuf inBuf
  setInAvail inLen
  setInNext $ unsafeForeignPtrToPtr inBuf `plusPtr` inOffset

isInputBufferEmpty :: M.Stream Bool
isInputBufferEmpty = (0 ==) <$> getInAvail

remainingInputBuffer :: M.Stream (ForeignPtr Word8, Int, Int)
remainingInputBuffer = do
  inBuf <- getInBuf
  inNext <- getInNext
  inAvail <- getInAvail

  assert (inAvail > 0) $ return ()

  return (inBuf, inNext `minusPtr` unsafeForeignPtrToPtr inBuf, inAvail)

pushOutputBuffer
  :: ForeignPtr Word8 -- ^
  -> Int -- ^
  -> Int -- ^
  -> M.Stream ()
pushOutputBuffer outBuf outOffset outLen = do
  outAvail <- getOutAvail
  assert (outAvail == 0) $ return ()

  oldOutBuf <- getOutBuf
  liftIO $ touchForeignPtr oldOutBuf

  setOutBuf outBuf
  setOutFree outLen
  setOutNext $ unsafeForeignPtrToPtr outBuf `plusPtr` outOffset

  setOutOffset outOffset
  setOutAvail 0

-- | Get the part of the output buffer that is currently full (might be 0, use
-- 'outputBufferBytesAvailable' to check). This may leave some space remaining
-- in the buffer, use 'outputBufferSpaceRemaining' to check.
popOutputBuffer :: M.Stream (ForeignPtr Word8, Int, Int)
popOutputBuffer = do
  outBuf <- getOutBuf
  outOffset <- getOutOffset
  outAvail <- getOutAvail

  assert (outAvail > 0) $ return ()

  setOutOffset $ outOffset + outAvail
  setOutAvail 0

  return (outBuf, outOffset, outAvail)

outputBufferBytesAvailable :: M.Stream Int
outputBufferBytesAvailable = getOutAvail

outputBufferSpaceRemaining :: M.Stream Int
outputBufferSpaceRemaining = getOutFree

isOutputBufferFull :: M.Stream Bool
isOutputBufferFull = (0 ==) <$> outputBufferSpaceRemaining

flushBuffers :: M.Stream ()
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

compress :: M.Stream Ret
compress = do
  undefined

decompress :: Action -> M.Stream Ret
decompress action = do
  outFree <- getOutFree

  assert (outFree > 0) $ return ()

  result <- code action
  outFree' <- getOutFree

  let outExtra = outFree - outFree'

  outAvail <- getOutAvail
  setOutAvail $ outAvail + outExtra
  return result

------------------------------------------------------------
-- M.Stream monad

-- | Opaque data type to hold the underlying state in the 'M.Stream' monad.
data State s = State
  { stateStream :: !Stream
  , stateInBuf :: !(ForeignPtr Word8)
  , stateOutBuf :: !(ForeignPtr Word8)
  , stateOutOffset :: !Int
  , stateOutLength :: !Int
  }

newState :: ST s (State s)
newState = unsafeIOToST $ do
  stream <- newStream

  -- TODO: Use LZMA_STREAM_INIT to initialize these fields
  withStream stream $ \p -> do
    {# set lzma_stream.next_in #} p nullPtr
    {# set lzma_stream.avail_in #} p 0
    {# set lzma_stream.total_in #} p 0

    {# set lzma_stream.next_out #} p nullPtr
    {# set lzma_stream.avail_out #} p 0
    {# set lzma_stream.total_out #} p 0

    -- internal fields
    {# set lzma_stream.internal #} p nullPtr
    {# set lzma_stream.allocator #} p nullPtr
    {# set lzma_stream.reserved_ptr1 #} p nullPtr
    {# set lzma_stream.reserved_ptr2 #} p nullPtr
    {# set lzma_stream.reserved_ptr3 #} p nullPtr
    {# set lzma_stream.reserved_ptr4 #} p nullPtr
    {# set lzma_stream.reserved_int1 #} p 0
    {# set lzma_stream.reserved_int2 #} p 0
    {# set lzma_stream.reserved_int3 #} p 0
    {# set lzma_stream.reserved_int4 #} p 0
    {# set lzma_stream.reserved_enum1 #} p $ fromReservedEnum ReservedEnum
    {# set lzma_stream.reserved_enum2 #} p $ fromReservedEnum ReservedEnum
  return $ State stream S.nullForeignPtr S.nullForeignPtr 0 0
  where
    fromReservedEnum :: Integral a => ReservedEnum -> a
    fromReservedEnum = fromIntegral . fromEnum

runStream :: M.Stream a -> State s -> ST s (a, State s)
runStream (M.Stream m) (State {..}) = unsafeIOToST $ do
  (inBuf, outBuf, outOff, outLen, a) <-
    m stateStream stateInBuf stateOutBuf stateOutOffset stateOutLength
  return (a, State stateStream inBuf outBuf outOff outLen)

getStream :: M.Stream Stream
getStream = M.Stream $ \stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, stream)

getInBuf :: M.Stream (ForeignPtr Word8)
getInBuf = M.Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, inBuf)

setInBuf :: ForeignPtr Word8 -> M.Stream ()
setInBuf inBuf = M.Stream $ \_stream _inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, ())

getOutBuf :: M.Stream (ForeignPtr Word8)
getOutBuf = M.Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, outBuf)

setOutBuf :: ForeignPtr Word8 -> M.Stream ()
setOutBuf outBuf = M.Stream $ \_stream inBuf _outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, ())

getOutOffset :: M.Stream Int
getOutOffset = M.Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, outOffset)

setOutOffset :: Int -> M.Stream ()
setOutOffset outOffset = M.Stream $ \_stream inBuf outBuf _ outLength ->
  return (inBuf, outBuf, outOffset, outLength, ())

getOutAvail :: M.Stream Int
getOutAvail = M.Stream $ \_stream inBuf outBuf outOffset outLength ->
  return (inBuf, outBuf, outOffset, outLength, outLength)

setOutAvail :: Int -> M.Stream ()
setOutAvail outLength = M.Stream $ \_stream inBuf outBuf outOffset _ ->
  return (inBuf, outBuf, outOffset, outLength, ())

------------------------------------------------------------

withStreamPtr :: (Ptr Stream -> IO a) -> M.Stream a
withStreamPtr f = do
  stream <- getStream
  liftIO $ withStream stream f

getInAvail :: M.Stream Int
getInAvail = do
  fromIntegral <$> withStreamPtr {# get lzma_stream.avail_in #}

setInAvail :: Int -> M.Stream ()
setInAvail n = withStreamPtr $ \p ->
  {# set lzma_stream.avail_in #} p (fromIntegral n)

getInNext :: M.Stream (Ptr Word8)
getInNext = castPtr <$> withStreamPtr {# get lzma_stream.next_in #}

setInNext :: Ptr Word8 -> M.Stream ()
setInNext p' = withStreamPtr $ \p ->
  {# set lzma_stream.next_in #} p (castPtr p')

getOutFree :: M.Stream Int
getOutFree =
  fromIntegral <$> withStreamPtr {# get lzma_stream.avail_out #}

setOutFree :: Int -> M.Stream ()
setOutFree n = withStreamPtr $ \p ->
  {# set lzma_stream.avail_out #} p (fromIntegral n)

#if DEBUG
getOutNext :: M.Stream (Ptr Word8)
getOutNext = castPtr <$> withStreamPtr {# get lzma_stream.next_out #}
#endif

setOutNext :: Ptr Word8 -> M.Stream ()
setOutNext p' = withStreamPtr $ \p ->
  {# set lzma_stream.next_out #} p (castPtr p')

-- | Initialize .xz M.Stream encoder using a preset number.
--
-- This function is intended for those who just want to use the basic features
-- if liblzma (that is, most developers out there).
easyEncoder
  :: Preset
  -- ^ Compression preset to use.
  --
  -- A preset consist of level number and zero or
  -- more flags. Usually flags aren't used, so preset is simply a number [0, 9]
  -- which match the options -0 ... -9 of the xz command line tool. Additional
  -- flags can be be set using '<>' from @Monoid@ with the preset level number,
  -- e.g. @'customPreset' 6 <> 'extremePreset'@.
  -> Check
  -- ^ Integrity check type to use. See 'Check' for available checks. The xz
  -- command line tool defaults to 'CheckCrc64', which is a good choice if you
  -- are unsure. 'CheckCrc32' is good too as long as the uncompressed file is
  -- not many gigabytes.
  -> M.Stream Ret
easyEncoder preset check = do
  s <- getStream
  liftIO $ lzma_easy_encoder s preset check

-- | Decode .xz Streams and .lzma files with autodetection.

-- This decoder autodetects between the .xz and .lzma file formats, and calls
-- 'lzma_stream_decoder' or 'lzma_alone_decoder' once the type of the input
-- file has been detected.
autoDecoder
  :: Word64
  -- ^ Memory usage limit as bytes. Use 'maxBound' to effectively disable the
  -- limiter.
  -> Flags
  -- ^ @<>@ of flags, or @mempty@ for no flags.
  -> M.Stream Ret
autoDecoder memLimit flags = do
  s <- getStream
  liftIO $ lzma_auto_decoder s memLimit flags

-- | Encode or decode data.
--
-- Once the 'M.Stream' has been successfully initialized, the actual encoding
-- or decoding is done using this function.
code :: Action -> M.Stream Ret
code action = do
#ifdef DEBUG
  outNext <- getOutNext
  outFree <- getOutFree
  assert (not (outNext == nullPtr && outFree > 0)) $ return ()
#endif
  s <- getStream
  liftIO $ lzma_code s action

-- | Free memory allocated for the coder data structures.
end :: M.Stream ()
end = getStream >>= liftIO . lzma_end

blockDecoder :: IndexIter -> Block -> IOVector Filter -> M.Stream Ret
blockDecoder IndexIter {..} block filters = do
  let IndexIterStream {indexIterStreamFlags} = indexIterStream
      IndexIterBlock {indexIterBlockUnpaddedSize} = indexIterBlock

  inNext <- getInNext

  liftIO $ do
    lzma_set_block_version block 0
    lzma_set_block_filters block filters

    check <- lzma_get_stream_flags_check indexIterStreamFlags
    lzma_set_block_check block check

    firstByte <- peek inNext
    lzma_set_block_header_size block $ lzma_block_header_size_decode firstByte

  inAvail <- getInAvail
  handleRet $ liftIO $ lzma_block_header_decode block inNext

  handleRet $
    liftIO $ lzma_block_compressed_size block indexIterBlockUnpaddedSize

  blockHeaderSize <- liftIO $ lzma_get_block_header_size block

  setInNext $ inNext `advancePtr` fromIntegral blockHeaderSize
  setInAvail $ inAvail - fromIntegral blockHeaderSize

  stream <- getStream
  liftIO $ lzma_block_decoder stream block
  where
    handleRet m = do
      ret <- m
      case ret of
        Error errorCode -> throwM (LZMAErrorCode errorCode)
        _ -> return ()

------------------------------------------------------------
-- Debugging tools

#ifdef DEBUG

debug :: Show a => a -> M.Stream ()
debug = liftIO . hPrint stderr

dump :: String -> M.Stream ()
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

consistencyCheck :: M.Stream ()
consistencyCheck = do
  outBuf <- getOutBuf
  outOffset <- getOutOffset
  outAvail <- getOutAvail
  outNext <- getOutNext

  let outBufPtr = unsafeForeignPtrToPtr outBuf

  assert (outBufPtr `plusPtr` (outOffset + outAvail) == outNext) $ return ()

#endif
