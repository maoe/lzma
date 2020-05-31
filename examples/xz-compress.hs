import System.Environment
import System.IO

import qualified Data.ByteString as S
import qualified Pipes.Internal as P

import Codec.Compression.LZMA.Incremental

main :: IO ()
main = do
  file:_ <- getArgs
  withFile file ReadMode cmpApp

cmpApp :: Handle -> IO ()
cmpApp h = go (compressIO defaultCompressParams)
  where
    go (P.Request () feed) = do
      chunk <- S.hGet h 2048
      hPutStrLn stderr $ "feeding " ++ show (S.length chunk) ++ " bytes of uncompressed data"
      go (feed chunk)
    go (P.Respond chunk next) = do
      hPutStrLn stderr $ "writing " ++ show (S.length chunk) ++ " bytes of compressed data"
      -- S.putStr chunk
      go (next ())
    go (P.M m) =
      m >>= go
    go (P.Pure _leftover) =
      return ()
