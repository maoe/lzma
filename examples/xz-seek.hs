{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Monad
import System.Environment
import System.IO

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Pipes.Core
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Codec.Compression.LZMA.Incremental

main :: IO ()
main = do
  file:args <- getArgs
  withFile file ReadMode $ \h -> do
    size <- hFileSize h
    runEffect $
      fromHandleRandom h +>> do
        (index, _padding) <- decodeIndexIO (fromIntegral size)
        seekableDecompressIO defaultDecompressParams index +>>
          case args of
            [] -> seekThenReadToEnd 0
            [pos] -> seekThenReadToEnd (read pos)
            positions -> mapM_ (seekAndDump . integerToPos . read) positions
  where
    integerToPos :: Integer -> Position 'Uncompressed
    integerToPos = fromIntegral

fromHandleRandom
  :: MonadIO m
  => Handle
  -> ReadRequest c
  -> Server (ReadRequest c) S.ByteString m ()
fromHandleRandom h = go
  where
    go req = case req of
      PRead pos -> do
        chunk <- liftIO $ do
          hSeek h AbsoluteSeek (fromIntegral pos)
          S.hGetSome h defaultChunkSize
        unless (S.null chunk) $ do
          req' <- respond chunk
          go req'
      Read -> do
        chunk <- liftIO $ S.hGetSome h defaultChunkSize
        unless (S.null chunk) $ do
          req' <- respond chunk
          go req'

seekThenReadToEnd :: Integer -> Client (ReadRequest 'Uncompressed) S.ByteString IO ()
seekThenReadToEnd pos = go $ PRead (fromIntegral pos)
  where
    go req = do
      chunk <- request req
      liftIO $ S8.putStr chunk
      go Read

seekAndDump
  :: Position 'Uncompressed
  -> Client (ReadRequest 'Uncompressed) S.ByteString IO ()
seekAndDump pos = do
  liftIO $ putStrLn $ "----------- Seeking to " ++ show pos
  chunk <- request $ PRead pos
  liftIO $ S8.putStrLn $ S.take 80 chunk
