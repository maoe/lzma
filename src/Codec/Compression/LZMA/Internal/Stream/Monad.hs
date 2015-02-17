{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Codec.Compression.LZMA.Internal.Stream.Monad where
import Control.Applicative
import Control.Monad
import Data.Typeable (Typeable, cast)
import Foreign

import Control.Monad.Catch
import Control.Monad.Trans (MonadIO(..))

import qualified Codec.Compression.LZMA.Internal.C as C

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

data SomeStreamException = forall e. Exception e => SomeStreamException e
  deriving Typeable

instance Show SomeStreamException where
  show (SomeStreamException e) = show e

instance Exception SomeStreamException

streamExceptionToException :: Exception e => e -> SomeException
streamExceptionToException = toException . SomeStreamException

streamExceptionFromException :: Exception e => SomeException -> Maybe e
streamExceptionFromException x = do
  SomeStreamException e <- fromException x
  cast e

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
