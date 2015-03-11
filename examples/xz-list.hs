{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Monad
import Data.Function (fix)
import Data.List (intercalate)
import System.Environment
import System.IO

import Codec.Compression.LZMA.Incremental
import Foreign.Var
import qualified Codec.Compression.LZMA.Internal.C as C

import Text.Printf

main :: IO ()
main = do
  file:_ <- getArgs
  (index, padding) <- withFile file ReadMode decodeIndex
  printInfoAdvanced index padding
  C.finalizeIndex index

printInfoAdvanced
  :: Index
  -> C.VLI -- ^ Stream padding
  -> IO ()
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
      number <- get $ C.indexIterStreamNumber iter
      blockCount <- get $ C.indexIterStreamBlockCount iter
      compressedOffset <- get $ C.indexIterStreamCompressedOffset iter
      uncompressedOffset <- get $ C.indexIterStreamUncompressedOffset iter
      compressedSize <- get $ C.indexIterStreamCompressedSize iter
      uncompressedSize <- get $ C.indexIterStreamUncompressedSize iter
      padding <- get $ C.indexIterStreamPadding iter
      let format = printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
      format
        (show number)
        (show blockCount)
        (show compressedOffset)
        (show uncompressedOffset)
        (show compressedSize)
        (show uncompressedSize)
        (show padding)
      loop

printBlocks :: Index -> IO ()
printBlocks index = do
  iter <- C.lzma_index_iter_init index
  fix $ \loop -> do
    notFound <- C.lzma_index_iter_next iter C.IndexIterBlockMode
    unless notFound $ do
      streamNumber <- get $ C.indexIterStreamNumber iter
      numberInStream <- get $ C.indexIterBlockNumberInFile iter
      compressedFileOffset <- get $ C.indexIterBlockCompressedFileOffset iter
      uncompressedFileOffset <- get $ C.indexIterBlockUncompressedFileOffset iter
      totalSize <- get $ C.indexIterBlockTotalSize iter
      uncompressedSize <- get $ C.indexIterBlockUncompressedSize iter
      let format = printf "%s\t%s\t%s\t%s\t%s\t%s\n"
      format
        (show streamNumber)
        (show numberInStream)
        (show compressedFileOffset)
        (show uncompressedFileOffset)
        (show totalSize)
        (show uncompressedSize)
      loop
