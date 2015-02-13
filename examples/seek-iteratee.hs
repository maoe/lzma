{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad.Trans
import System.Environment
import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Codec.Compression.LZMA.Internal
import Data.Iteratee

main :: IO ()
main = do
  file:_ <- getArgs
  flip fileDriver file $
    joinI $ enumDecompress defaultDecompressParams $ mapChunksM_ S8.putStrLn

enumDecompress
  :: MonadIO m
  => DecompressParams
  -> Enumeratee S.ByteString S.ByteString m a
enumDecompress = flip go . decompressIO
  where
    go iter (DecompressInputRequired feed) = do
      chunk <- getChunk
      stream <- liftIO $ feed chunk
      go iter stream
    go iter (DecompressOutputAvailable chunk next) = do
      stream' <- liftIO next
      go (joinIM $ enumPure1Chunk chunk iter) stream'
    go iter (DecompressStreamEnd rest) =
      -- FIXME: What should we do about the leftover?
      return iter
    go iter (DecompressStreamError err) = throwErr $ toException err

enumDecompressRandom
  :: forall m a. MonadIO m
  => DecompressParams
  -> Enumeratee S.ByteString S.ByteString m a
enumDecompressRandom params iter = seekLoop iter (decompressSeekableIO params)
  where
    seekLoop iter (SeekableDecompressStream seek) = undefined