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
      -> ForeignPtr Word8
      -> ForeignPtr Word8
      -> Int
      -> Int
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
  Stream m >>= k = Stream $ \stream inBuf outBuf offset len -> do
    (inBuf', outBuf', offset', len', a) <- m stream inBuf outBuf offset len
    unStream (k a) stream inBuf' outBuf' offset' len'

instance MonadIO Stream where
  liftIO io = Stream $ \_ inBuf outBuf offset len -> do
    a <- io
    return (inBuf, outBuf, offset, len, a)

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
  throwM = liftIO . throwM

instance MonadCatch Stream where
  catch (Stream m) handler =
    Stream $ \stream inBuf outBuf offset len ->
      m stream inBuf outBuf offset len
        `catch` \e ->
          unStream (handler e) stream inBuf outBuf offset len

instance MonadMask Stream where
  mask f = Stream $ \stream inBuf outBuf offset len ->
    mask $ \restore ->
      unStream (f $ g restore) stream inBuf outBuf offset len
    where
      g :: (forall b. IO b -> IO b) -> Stream a -> Stream a
      g restore (Stream m) = Stream $ \stream inBuf outBuf offset len ->
        restore $ m stream inBuf outBuf offset len

  uninterruptibleMask f = Stream $ \stream inBuf outBuf offset len ->
    uninterruptibleMask $ \restore ->
      unStream (f $ g restore) stream inBuf outBuf offset len
    where
      g :: (forall b. IO b -> IO b) -> Stream a -> Stream a
      g restore (Stream m) = Stream $ \stream inBuf outBuf offset len ->
        restore $ m stream inBuf outBuf offset len
