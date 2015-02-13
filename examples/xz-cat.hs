import System.Environment
import System.IO

import qualified Data.ByteString as S
import qualified Pipes.Internal as P

import Codec.Compression.LZMA

main :: IO ()
main = do
  file:_ <- getArgs
  withFile file ReadMode dumpApp

dumpApp :: Handle -> IO ()
dumpApp h = go (decompressIO defaultDecompressParams)
  where
    go (P.Request () feed) = do
      chunk <- S.hGet h 2048
      go (feed chunk)
    go (P.Respond chunk next) = do
      S.putStr chunk
      go (next ())
    go (P.M m) =
      m >>= go
    go (P.Pure _leftover) =
      return ()
