{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Monad
import Data.Function (fix)
import Data.List (intercalate)
import Foreign
import System.Environment
import System.IO

import Codec.Compression.LZMA.Internal
import Codec.Compression.LZMA.Internal.C (Index, IndexIter(..))
import qualified Codec.Compression.LZMA.Internal.C as C

import Text.Printf

main :: IO ()
main = do
  file:_ <- getArgs
  (index, padding) <- withFile file ReadMode decodeIndex
  printInfoAdvanced index padding

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
