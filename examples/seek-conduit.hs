import Control.Exception
import Control.Monad.Trans
import System.Environment
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Codec.Compression.LZMA.Internal hiding (decompress)
import Control.Monad.Catch
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
  file:_ <- getArgs
  runResourceT $
    CB.sourceFile file $$
      decompress defaultDecompressParams =$ CL.mapM_ (liftIO . S8.putStrLn)

decompress
  :: (MonadIO m, MonadThrow m)
  => DecompressParams
  -> Conduit S.ByteString m S.ByteString
decompress = go . decompressIO
  where
    go s@(DecompressInputRequired feed) = do
      chunk'm <- await
      case chunk'm of
        Nothing -> go s
        Just chunk -> do
          stream <- liftIO $ feed chunk
          go stream
    go (DecompressOutputAvailable chunk next) = do
      stream <- liftIO next
      yield chunk
      go stream
    go (DecompressStreamEnd rest) = leftover rest
