{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Codec.Compression.LZMA.Internal.IndexDecoder
  ( IndexDecoder
  , runIndexDecoder
  , IndexDecoderState
  , newIndexDecoderState
  , getPosition
  , setPosition
  , modifyPosition'
  , getStreamPadding
  , setStreamPadding
  , modifyStreamPadding'
  , getStreamHeader
  , getStreamFooter
  ) where
import Control.Applicative
import Control.Monad

import Control.Monad.Catch
import Control.Monad.Trans

import Codec.Compression.LZMA.Internal.Types
import qualified Codec.Compression.LZMA.Internal.C as C

-- | Index decoder monad.
newtype IndexDecoder a = IndexDecoder
  { unIndexDecoder
      :: C.StreamFlags -- Stream header
      -> C.StreamFlags -- Stream footer
      -> Position 'Compressed
      -> C.VLI -- Stream padding
      -> IO (Position 'Compressed, C.VLI, a)
  } deriving Functor

instance Applicative IndexDecoder where
  pure = return
  (<*>) = ap

instance Monad IndexDecoder where
  return a = IndexDecoder $ \_header _footer pos padding ->
    return (pos, padding, a)
  IndexDecoder m >>= k = IndexDecoder $ \header footer pos padding -> do
    (pos', padding', a) <- m header footer pos padding
    unIndexDecoder (k a) header footer pos' padding'

instance MonadIO IndexDecoder where
  liftIO io = IndexDecoder $ \_ _ pos padding -> do
    a <- io
    return (pos, padding, a)

instance MonadThrow IndexDecoder where
  throwM = liftIO . throwM

instance MonadCatch IndexDecoder where
  catch (IndexDecoder m) handler =
    IndexDecoder $ \header footer pos padding ->
      m header footer pos padding
        `catch` \e ->
          unIndexDecoder (handler e) header footer pos padding

data IndexDecoderState = IndexDecoderState
  { indexDecoderPosition :: !(Position 'Compressed)
  -- ^ Decoder's current position in the compressed file.
  , indexDecoderStreamPadding :: !C.VLI
  -- ^ Total size of stream paddings.
  } deriving Show

-- | Create an initial decoder state.
newIndexDecoderState :: IndexDecoderState
newIndexDecoderState = IndexDecoderState 0 0

-- | Run the index decoder
runIndexDecoder
  :: IndexDecoder a
  -> IndexDecoderState
  -> C.StreamFlags -- ^ Stream header
  -> C.StreamFlags -- ^ Stream footer
  -> IO (IndexDecoderState, a)
runIndexDecoder decoder IndexDecoderState {..} header footer = do
  (pos, padding, a) <- unIndexDecoder decoder header footer
    indexDecoderPosition
    indexDecoderStreamPadding
  return (IndexDecoderState pos padding, a)

getPosition :: IndexDecoder (Position 'Compressed)
getPosition = IndexDecoder $ \_ _ pos padding ->
  return (pos, padding, pos)

setPosition :: Position 'Compressed -> IndexDecoder ()
setPosition !pos = IndexDecoder $ \_ _ _pos padding ->
  return (pos, padding, ())

modifyPosition'
  :: (Position 'Compressed -> Position 'Compressed)
  -> IndexDecoder ()
modifyPosition' f = do
  pos <- getPosition
  setPosition $! f pos

getStreamPadding :: IndexDecoder C.VLI
getStreamPadding = IndexDecoder $ \_ _ pos padding ->
  return (pos, padding, padding)

setStreamPadding :: C.VLI -> IndexDecoder ()
setStreamPadding padding = IndexDecoder $ \_ _ pos _padding ->
  return (pos, padding, ())

modifyStreamPadding' :: (C.VLI -> C.VLI) -> IndexDecoder ()
modifyStreamPadding' f = do
  padding <- getStreamPadding
  setStreamPadding $! f padding

getStreamHeader, getStreamFooter :: IndexDecoder C.StreamFlags
getStreamHeader = IndexDecoder $ \header _footer pos padding ->
  return (pos, padding, header)
getStreamFooter = IndexDecoder $ \_header footer pos padding ->
  return (pos, padding, footer)
