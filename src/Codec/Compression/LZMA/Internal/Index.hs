{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Codec.Compression.LZMA.Internal.Index
  ( Index
  , IndexIter(..)
  , IndexIterStream(..)
  , IndexIterBlock(..)

  -- * Decoding indicies
  , IndexDecoding
  , runIndexDecoding
  , indexDecodingToIO
  , DecoderState, newDecoderState

  -- ** Types
  , SeekStream
  , runSeekStream
  , ReadRequest(..)
  , Position
  , Compression(..)
  , Size
  , StreamPadding

  , parseIndicies
  , parseIndex

  -- ** Exceptions
  , SeekException(..)
  ) where
import Control.Applicative
import Control.Exception (IOException)
import Control.Monad
import Data.Typeable (Typeable)
import Foreign
import System.IO

import Control.Monad.Catch
import Control.Monad.Trans
import Pipes.Core
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L (defaultChunkSize)
import qualified Pipes.Internal as P

import Codec.Compression.LZMA.Internal.C
------------------------------------------------------------
-- Parsing indicies

-- | Index decoder monad.
newtype IndexDecoding a = IndexDecoding
  { unIndexDecoding
      :: StreamFlags -- Stream header
      -> StreamFlags -- Stream footer
      -> Position 'Compressed
      -> StreamPadding
      -> IO (Position 'Compressed, StreamPadding, a)
  } deriving Functor

instance Applicative IndexDecoding where
  pure = return
  (<*>) = ap

instance Monad IndexDecoding where
  return a = IndexDecoding $ \_header _footer pos padding ->
    return (pos, padding, a)
  IndexDecoding m >>= k = IndexDecoding $ \header footer pos padding -> do
    (pos', padding', a) <- m header footer pos padding
    unIndexDecoding (k a) header footer pos' padding'

instance MonadIO IndexDecoding where
  liftIO io = IndexDecoding $ \_ _ pos padding -> do
    a <- io
    return (pos, padding, a)

instance MonadThrow IndexDecoding where
  throwM = liftIO . throwM

instance MonadCatch IndexDecoding where
  catch (IndexDecoding m) handler =
    IndexDecoding $ \header footer pos padding ->
      m header footer pos padding
        `catch` \e ->
          unIndexDecoding (handler e) header footer pos padding

data DecoderState = DecoderState
  { decoderPosition :: !(Position 'Compressed)
  , decoderStreamPadding :: !StreamPadding
  } deriving Show

newDecoderState :: DecoderState
newDecoderState = DecoderState 0 0

-- | Run the index decoder. The resulting 'VLI' is the total length of the
-- stream paddings.
runIndexDecoding
  :: IndexDecoding a
  -> DecoderState
  -> StreamFlags -- ^ Stream header
  -> StreamFlags -- ^ Stream footer
  -> IO (DecoderState, a)
runIndexDecoding decoder DecoderState {..} header footer = do
  (pos, padding, a) <-
    unIndexDecoding decoder header footer decoderPosition decoderStreamPadding
  return (DecoderState pos padding, a)

getPosition :: IndexDecoding (Position 'Compressed)
getPosition = IndexDecoding $ \_ _ pos padding ->
  return (pos, padding, pos)

setPosition :: Position 'Compressed -> IndexDecoding ()
setPosition !pos = IndexDecoding $ \_ _ _pos padding ->
  return (pos, padding, ())

modifyPosition'
  :: (Position 'Compressed -> Position 'Compressed)
  -> IndexDecoding ()
modifyPosition' f = do
  pos <- getPosition
  setPosition $! f pos

getStreamPadding :: IndexDecoding VLI
getStreamPadding = IndexDecoding $ \_ _ pos padding ->
  return (pos, padding, padding)

setStreamPadding :: VLI -> IndexDecoding ()
setStreamPadding padding = IndexDecoding $ \_ _ pos _padding ->
  return (pos, padding, ())

modifyStreamPadding' :: (VLI -> VLI) -> IndexDecoding ()
modifyStreamPadding' f = do
  padding <- getStreamPadding
  setStreamPadding $! f padding

getStreamHeader, getStreamFooter :: IndexDecoding StreamFlags
getStreamHeader = IndexDecoding $ \header _footer pos padding ->
  return (pos, padding, header)
getStreamFooter = IndexDecoding $ \_header footer pos padding ->
  return (pos, padding, footer)

-- | Seek to an absolute position and ask for an input with given size.
pread
  :: Size
  -- ^ Input size
  -> SeekStream IndexDecoding S.ByteString
pread size = do
  pos <- lift getPosition
  request $ PReadWithSize pos size

------------------------------------------------------------

-- | Absolute position in a file.
newtype Position (c :: Compression) = Position Integer
  deriving (Eq, Enum, Ord, Real, Num, Integral)

instance Show (Position c) where
  show (Position pos) = show pos

data Compression = Compressed | Uncompressed
  deriving (Eq, Show)

-- | File size
type Size = Int
type StreamPadding = VLI

data ReadRequest (c :: Compression) where
  PReadWithSize :: Position 'Compressed -> Size -> ReadRequest 'Compressed
  --- ^ Seek to the position and read the given number of bytes. This constructor
  -- is used only for asking for compressed bytes.
  PRead :: Position c -> ReadRequest c
  --- ^ Seek to the position and read bytes.
  Read :: ReadRequest c
  --- ^ Read bytes from the current position.

deriving instance Show (ReadRequest c)

-- | Unfolding of seek process.
type SeekStream = Client (ReadRequest 'Compressed) S.ByteString

runSeekStream :: (MonadIO m, MonadThrow m) => Handle -> SeekStream m a -> m a
runSeekStream h = runEffect . loop
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
        show expectedLen ++ " bytes, but got " ++ show actualLen ++ " bytes)"

-- | Seek operation failure in downstream.
data SeekException = SeekError ErrorCode deriving (Show, Typeable)

instance Exception SeekException

------------------------------------------------------------

headerSize, footerSize :: Integral a => a
headerSize = fromIntegral streamHeaderSize
footerSize = headerSize

-- | Seek thorough a seekable stream and build a combined index. The index can
-- later be used for seeking.
parseIndicies
  :: Size -- ^ Size of the file
  -> SeekStream IndexDecoding (Index, StreamPadding)
parseIndicies fileSize = do
  lift $ setPosition $ fromIntegral fileSize
  index <- parseIndex
  loop index
  padding <- lift getStreamPadding
  return (index, padding)
  where
    loop :: Index -> SeekStream IndexDecoding ()
    loop index = do
      pos <- lift getPosition
      when (pos > 0) $ do
        index' <- parseIndex
        handleRet $ liftIO $ lzma_index_cat index index' Nothing
        loop index

-- | Parse an index
parseIndex :: SeekStream IndexDecoding Index
parseIndex = do
  padding <- decodeStreamFooter
  index <- decodeIndex 8192 -- FIXME: Set appropreate size
  decodeStreamHeader index
  checkIntegrity index
  handleRet $ liftIO $ lzma_index_stream_padding index padding
  lift $ modifyStreamPadding' (+ padding)
  return index

decodeStreamFooter :: SeekStream IndexDecoding StreamPadding
decodeStreamFooter = loop 0
  where
    loop padding = do
      pos <- lift getPosition
      when (pos < 2 * headerSize) $
        lift $ throwM $ SeekError DataError

      pos' <- lift $ do
        modifyPosition' (subtract footerSize)
        getPosition
      when (pos' < footerSize) $
        lift $ throwM $ SeekError DataError

      chunk <- pread footerSize
      if isStreamPadding $ dropStreamPadding 2 chunk
        then do
          padding' <- lift $ skipStreamPaddings chunk 2 padding
          loop padding'
        else do
          footer <- lift getStreamFooter
          handleRet $ liftIO $ do
            let S.PS inFPtr _off _len = chunk
            withForeignPtr inFPtr $ lzma_stream_footer_decode footer
          -- TODO: check the version of the stream footer
          return padding

skipStreamPaddings
  :: S.ByteString
  -- ^ Input chunk
  -> Int
  -- ^ Index
  -> StreamPadding
  -> IndexDecoding StreamPadding
skipStreamPaddings chunk = go
  where
    go !i !padding =
      if i >= 0 && isStreamPadding (dropStreamPadding i chunk)
        then do
          modifyPosition' (subtract 4)
          go (i - 1) (padding + 4)
        else return padding

isStreamPadding :: S.ByteString -> Bool
isStreamPadding = S.all (== 0) . S.take 4

dropStreamPadding :: Int -> S.ByteString -> S.ByteString
dropStreamPadding n = S.drop (4*n)

getIndexSize :: IndexDecoding VLI
getIndexSize = do
  footer <- getStreamFooter
  liftIO $ lzma_stream_flags_backward_size footer

-- | Decode a stream index.
decodeIndex
  :: VLI -- ^ Buffer size
  -> SeekStream IndexDecoding Index
decodeIndex bufSize = do
  indexSize <- lift getIndexSize
  -- Set posision to the beginning of the index.
  lift $ modifyPosition' $ subtract $ fromIntegral indexSize
  stream <- liftIO newStream
  (ret, indexRef) <- liftIO $ lzma_index_decoder stream maxBound -- FIXME: Set proper value
  unless (ret == Ok) $ lift $ throwM $ SeekError ProgError

  loop stream indexRef indexSize

  liftIO $ do
    allocator <- lzma_get_stream_allocator stream
    peekIndexRef indexRef allocator
  where
    loop stream indexRef indexSize = do
      let inAvail :: Integral a => a
          inAvail = fromIntegral $ min bufSize indexSize
      liftIO $ lzma_set_stream_avail_in stream inAvail
      chunk <- pread inAvail
      lift $ modifyPosition' (+ inAvail)
      let indexSize' = indexSize - inAvail
      liftIO $ do
        let S.PS inFPtr _offset _len = chunk
        withForeignPtr inFPtr $ lzma_set_stream_next_in stream
      ret <- liftIO $ lzma_code stream Run
      case ret of
        Ok -> loop stream indexRef indexSize'
        Error reason -> lift $ throwM $ SeekError reason
        StreamEnd -> do
          inAvail' <- liftIO $ lzma_get_stream_avail_in stream
          unless (indexSize' == 0 && inAvail' == 0) $
            lift $ throwM $ SeekError DataError

-- | Decode the stream header and check that its stream flags match the stream
-- footer.
decodeStreamHeader
  :: Index
  -> SeekStream IndexDecoding ()
decodeStreamHeader index = do
  indexSize <- lift getIndexSize
  lift $ modifyPosition' (subtract $ fromIntegral indexSize + headerSize)
  blocksSize <- liftIO $ lzma_index_total_size index
  lift $ modifyPosition' (subtract $ fromIntegral blocksSize)
  chunk <- pread headerSize
  let S.PS inFPtr _off _len = chunk
  header <- lift getStreamHeader
  handleRet $ liftIO $ withForeignPtr inFPtr $ lzma_stream_header_decode header

checkIntegrity
  :: Index
  -> SeekStream IndexDecoding ()
checkIntegrity index = do
  header <- lift getStreamHeader
  footer <- lift getStreamFooter
  handleRet $ liftIO $ lzma_stream_flags_compare header footer
  handleRet $ liftIO $ lzma_index_stream_flags index footer
  padding <- lift getStreamPadding
  handleRet $ liftIO $ lzma_index_stream_padding index padding

handleRet :: MonadThrow m => SeekStream m Ret -> SeekStream m ()
handleRet m = do
  ret <- m
  unless (ret == Ok) $ lift $ throwM $ SeekError ProgError

indexDecodingToIO
  :: SeekStream IndexDecoding a
  -> DecoderState
  -> StreamFlags -- ^ Stream header
  -> StreamFlags -- ^ Stream footer
  -> SeekStream IO a
indexDecodingToIO stream0 state0 header footer = go stream0 state0
  where
    go (P.Request req next) state =
      P.Request req $ \chunk -> go (next chunk) state
    go (P.Respond out next) state =
      P.Respond out $ \resp -> go (next resp) state
    go (P.M m) state = do
      (state', stream') <- liftIO $ runIndexDecoding m state header footer
      go stream' state'
    go (P.Pure a) _ =  P.Pure a
