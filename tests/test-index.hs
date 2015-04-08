{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Data.Word
import Foreign
import Text.Printf (printf)

import Foreign.Var
import Test.Tasty.HUnit
import Test.Tasty.TH

import Codec.Compression.LZMA.Internal.C

main :: IO ()
main = $defaultMainGenerator

memLimit :: Word64
memLimit = 2^(20 :: Int)

-- smallCount :: Integral a => a
-- smallCount = 3

bigCount :: Integral a => a
bigCount = 5555

assertNonNullIndex :: String -> Index -> Assertion
assertNonNullIndex label (Index indexPtr) =
  assertBool label $ indexPtr /= nullPtr

createEmpty :: IO Index
createEmpty = do
  index <- indexInit
  assertNonNullIndex "indexInit" index
  return index

createSmall :: IO Index
createSmall = do
  index <- indexInit
  do
    ret <- indexAppend index 101 555
    assertEqual "indexAppend index Nothing 101 555" Ok ret
  do
    ret <- indexAppend index 602 777
    assertEqual "indexAppend index Nothing 602 777" Ok ret
  do
    ret <- indexAppend index 804 999
    assertEqual "indexAppend index Nothing 804 999" Ok ret
  return index

createBig :: IO Index
createBig = do
  index <- indexInit
  (totalSize, uncompressedSize) <- go index 0 11 0 0
  do
    count <- indexBlockCount index
    assertEqual "block count" bigCount count
  do
    size <- indexTotalSize index
    assertEqual "index total size" totalSize size
  do
    size <- indexUncompressedSize index
    assertEqual "index uncompressed size" uncompressedSize size
  do
    indexSize' <- indexSize index
    streamSize <- indexStreamSize index
    assertEqual "stream size"
      (totalSize + indexSize' + 2 * streamHeaderSize)
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
            ret <- indexAppend index (fromIntegral t) (fromIntegral n)
            let message = printf "go.indexAppend %d %d [j=%d]" t n j
            assertEqual message Ok ret
          let !totalSize' = totalSize + ((fromIntegral t + 3) .&. complement 3)
              !uncompressedSize' = uncompressedSize + fromIntegral n
          go index (j + 1) n totalSize' uncompressedSize'

case_overflow :: Assertion
case_overflow = do
  index <- createEmpty
  ret <- indexAppend index (maxBound - 5) 1234
  assertEqual "indexAppend" (Error DataError) ret

testMany :: Index -> Assertion
testMany index = do
  testCopy index
  testRead index
  testCode index

testCopy :: Index -> Assertion
testCopy index = do
  index' <- indexDup index
  assertNonNullIndex "indexDup" index'
  -- TODO: Implement isEqualIndex

testRead :: Index -> Assertion
testRead index = do
  iter <- indexIterInit index
  go iter 0
  where
    go :: IndexIter -> Int -> IO ()
    go iter j = when (j < 2) $ do
      (totalSize, uncompressedSize, count) <- next iter 0 0 streamHeaderSize 0 0

      do
        actual <- indexTotalSize index
        assertEqual "indexTotalSize" totalSize actual
      do
        actual <- indexUncompressedSize index
        assertEqual "indexUncompressedSize" uncompressedSize actual
      do
        actual <- indexBlockCount index
        assertEqual "indexBlockCount" count actual

      indexIterRewind iter
      go iter $ j + 1

    next iter !size !uncompSize !offset !uncompOffset !count = do
      notFound <- indexIterNext iter IndexIterBlockMode
      if notFound
        then return (size, uncompSize, count)
        else do
          size' <- get $ indexIterBlockTotalSize iter
          uncompSize' <- get $ indexIterBlockUncompressedSize iter
          offset' <- get $ indexIterBlockCompressedFileOffset iter
          uncompOffset' <- get $ indexIterBlockUncompressedFileOffset iter
          next iter
            (size + size')
            (uncompSize + uncompSize')
            (offset + offset')
            (uncompOffset + uncompOffset')
            (count + 1)

testCode :: Index -> Assertion
testCode index = do
  let allocSize = 128 * 1024
  buf <- mallocForeignPtrBytes allocSize
  stream <- newStream

  do -- encode
    ret <- indexEncoder stream index
    assertEqual "indexEncoder" Ok ret
    size <- indexSize index
    encodeLoop stream buf size

  do -- decode
    (ret, indexFPtr) <- indexDecoder stream memLimit
    assertEqual "indexDecoder's return value" Ok ret
    withForeignPtr indexFPtr $ \indexPtr -> do
      Index ptr <- peek indexPtr
      assertEqual "index created by indexDecoder" nullPtr ptr

  return ()

encodeLoop :: Stream -> ForeignPtr Word8 -> VLI -> IO ()
encodeLoop stream outFPtr outSize = do
  let outLeft = if outSize > 0 then outSize + 1 else 0
  withForeignPtr outFPtr $ loop outLeft
  where
    loop outLeft outPtr
      | outLeft > 0 = do
        streamNextOut stream $= outPtr
        streamAvailOut stream $= 1
        ret <- code stream Run
        case ret of
          Ok -> loop (outLeft - 1) (advancePtr outPtr 1)
          StreamEnd -> return ()
          Error reason -> assertFailure $ "code: " ++ show reason

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
