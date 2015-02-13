{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Data.Word
import Foreign
import Text.Printf (printf)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Codec.Compression.LZMA.Internal.C

import Debug.Trace

main :: IO ()
main = $defaultMainGenerator

memLimit :: Word64
memLimit = 2^20

smallCount :: Integral a => a
smallCount = 3

bigCount :: Integral a => a
bigCount = 5555

assertNonNullIndex :: String -> Index -> Assertion
assertNonNullIndex label index =
  withIndex index $ \indexPtr ->
    assertBool label $ indexPtr /= nullPtr

createEmpty :: IO Index
createEmpty = do
  index <- lzma_index_init Nothing
  assertNonNullIndex "lzma_index_init" index
  return index

createSmall :: IO Index
createSmall = do
  index <- lzma_index_init Nothing
  do
    ret <- lzma_index_append index Nothing 101 555
    assertEqual "lzma_index_append index Nothing 101 555" Ok ret
  do
    ret <- lzma_index_append index Nothing 602 777
    assertEqual "lzma_index_append index Nothing 602 777" Ok ret
  do
    ret <- lzma_index_append index Nothing 804 999
    assertEqual "lzma_index_append index Nothing 804 999" Ok ret
  return index

createBig :: IO Index
createBig = do
  index <- lzma_index_init Nothing
  (totalSize, uncompressedSize) <- go index 0 11 0 0
  do
    count <- lzma_index_block_count index
    assertEqual "block count" bigCount count
  do
    size <- lzma_index_total_size index
    assertEqual "index total size" totalSize size
  do
    size <- lzma_index_uncompressed_size index
    assertEqual "index uncompressed size" uncompressedSize size
  do
    indexSize <- lzma_index_size index
    streamSize <- lzma_index_stream_size index
    assertEqual "stream size"
      (totalSize + indexSize + 2 * streamHeaderSize)
      streamSize
  return index
  where
    go :: Index -> Int -> Word32 -> VLI -> VLI -> IO (VLI, VLI)
    go index j n0 totalSize uncompressedSize
      | j >= bigCount = return (totalSize, uncompressedSize)
      | otherwise = do
          let !n = 7019 * n0 + 7607
              !t = n * 3011
          do
            ret <- lzma_index_append index Nothing (fromIntegral t) (fromIntegral n)
            let message = printf "go.lzma_index_append %d %d [j=%d]" t n j
            assertEqual message Ok ret
          let !totalSize' = totalSize + ((fromIntegral t + 3) .&. complement 3)
              !uncompressedSize' = uncompressedSize + fromIntegral n
          go index (j + 1) n totalSize' uncompressedSize'

case_overflow :: Assertion
case_overflow = do
  index <- createEmpty
  ret <- lzma_index_append index Nothing (maxBound - 5) 1234
  assertEqual "lzma_index_append" (Error DataError) ret

testMany :: Index -> Assertion
testMany index = do
  testCopy index
  testRead index
  testCode index

testCopy :: Index -> Assertion
testCopy index = do
  index' <- lzma_index_dup index Nothing
  assertNonNullIndex "lzma_index_dup" index'
  -- TODO: Implement isEqualIndex

testRead :: Index -> Assertion
testRead index = do
  iter <- lzma_index_iter_init index
  go iter 0
  where
    go iter j = when (j < 2) $ do
      (totalSize, uncompressedSize, count) <- next iter 0 0 streamHeaderSize 0 0

      do
        actual <- lzma_index_total_size index
        assertEqual "lzma_index_total_size" totalSize actual
      do
        actual <- lzma_index_uncompressed_size index
        assertEqual "lzma_index_uncompressed_size" uncompressedSize actual
      do
        actual <- lzma_index_block_count index
        assertEqual "lzma_index_block_count" count actual

      lzma_index_iter_rewind iter
      go iter $ j + 1

    next iter !size !uncompSize !offset !uncompOffset !count = do
      notFound <- lzma_index_iter_next iter IndexIterBlockMode
      if notFound
        then return (size, uncompSize, count)
        else do
          IndexIter {indexIterBlock} <- withForeignPtr iter peek
          let IndexIterBlock {..} = indexIterBlock
          next iter
            (size + indexIterBlockTotalSize)
            (uncompSize + indexIterBlockUncompressedSize)
            (offset + indexIterBlockCompressedFileOffset)
            (uncompOffset + indexIterBlockUncompressedFileOffset)
            (count + 1)

testCode :: Index -> Assertion
testCode index = do
  let allocSize = 128 * 1024
  buf <- mallocForeignPtrBytes allocSize
  stream <- newStream

  do -- encode
    ret <- lzma_index_encoder stream index
    assertEqual "lzma_index_encoder" Ok ret
    indexSize <- lzma_index_size index
    encodeLoop stream buf indexSize

  do -- decode
    (ret, indexRef)<- lzma_index_decoder stream memLimit
    assertEqual "lzma_index_decoder's return value" Ok ret
    withIndexRef indexRef $ \index ->
      withIndex index $ assertEqual "index created by lzma_index_decoder" nullPtr
    return ()

  return ()

encodeLoop :: Stream -> ForeignPtr Word8 -> VLI -> IO ()
encodeLoop stream outFPtr outSize = do
  let outLeft = if outSize > 0 then outSize + 1 else 0
  withForeignPtr outFPtr $ loop outLeft
  where
    loop outLeft outPtr
      | outLeft > 0 = do
        lzma_set_stream_next_out stream outPtr
        lzma_set_stream_avail_out stream 1
        ret <- lzma_code stream Run
        case ret of
          Ok -> loop (outLeft - 1) (advancePtr outPtr 1)
          StreamEnd -> return ()
          Error reason -> assertFailure $ "lzma_code: " ++ show reason

case_testMany_empty :: Assertion
case_testMany_empty = do
  index <- createEmpty
  testMany index

case_testMany_small :: Assertion
case_testMany_small = do
  index <- createSmall
  testMany index

case_testMany_big :: Assertion
case_testMany_big = do
  index <- createBig
  testMany index
