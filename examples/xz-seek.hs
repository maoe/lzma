{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Monad
import Data.Function (fix)
import Data.List (intercalate)
import Foreign
import System.Environment
import System.IO

import Control.Monad.Trans (MonadIO(..))
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Pipes.Core
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Codec.Compression.LZMA.Incremental
import qualified Codec.Compression.LZMA.Internal.C as C

import Text.Printf

main :: IO ()
main = do
  file:args <- getArgs
  (index, _padding) <- withFile file ReadMode decodeIndicies
  withFile file ReadMode $ \h -> runEffect $
    fromHandleRandom h +>>
    seekableDecompressIO defaultDecompressParams index +>>
    case args of
      [] -> seekThenReadToEnd 0
      [pos] -> seekThenReadToEnd (read pos)
      positions -> mapM_ seekAndDump (map (integerToPos . read) positions)
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
      PReadWithSize pos size -> do
        chunk <- liftIO $ do
          hSeek h AbsoluteSeek (fromIntegral pos)
          S.hGet h size
        req' <- respond chunk
        go req'
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

printInfoAdvanced :: Index -> C.VLI -> IO ()
printInfoAdvanced index padding = do
  streamCount <- C.lzma_index_stream_count index
  blockCount <- C.lzma_index_block_count index
  fileSize <- C.lzma_index_file_size index
  uncompressedSize <- C.lzma_index_uncompressed_size index
  checks <- C.lzma_index_checks index
  printf "Streams:           %d\n" (fromIntegral streamCount :: Int)
  printf "Blocks:            %d\n" (fromIntegral blockCount :: Int)
  printf "Compressed size:   %d bytes\n" (fromIntegral fileSize :: Int)
  printf "Uncompressed size: %d bytes\n" (fromIntegral uncompressedSize :: Int)
  printf "Checks:            %s\n" $ intercalate ", " $ map show checks
  printf "Stream padding:    %s\n" (show padding)

  printStreams index
  printBlocks index

printStreams :: Index -> IO ()
printStreams index = do
  iter <- C.lzma_index_iter_init index
  fix $ \loop -> do
    notFound <- C.lzma_index_iter_next iter C.IndexIterStreamMode
    unless notFound $ do
      C.IndexIter {..} <- withForeignPtr iter peek
      let C.IndexIterStream {..} = indexIterStream
      let format = printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
      format
        (show indexIterStreamNumber)
        (show indexIterStreamBlockCount)
        (show indexIterStreamCompressedOffset)
        (show indexIterStreamUncompressedOffset)
        (show indexIterStreamCompressedSize)
        (show indexIterStreamUncompressedSize)
        (show indexIterStreamPadding)
      loop

printBlocks :: Index -> IO ()
printBlocks index = do
  iter <- C.lzma_index_iter_init index
  fix $ \loop -> do
    notFound <- C.lzma_index_iter_next iter C.IndexIterBlockMode
    unless notFound $ do
      C.IndexIter {..} <- withForeignPtr iter peek
      let C.IndexIterStream {indexIterStreamNumber} = indexIterStream
      let C.IndexIterBlock {..} = indexIterBlock
      let format = printf "%s\t%s\t%s\t%s\t%s\t%s\n"
      format
        (show indexIterStreamNumber)
        (show indexIterBlockNumberInStream)
        (show indexIterBlockCompressedFileOffset)
        (show indexIterBlockUncompressedFileOffset)
        (show indexIterBlockTotalSize)
        (show indexIterBlockUncompressedSize)
      loop
