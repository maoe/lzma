{-# LANGUAGE TemplateHaskell #-}
import Foreign

import Data.Vector.Storable.Mutable (IOVector)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Data.Vector.Storable.Mutable as VM

import Codec.Compression.LZMA.Internal.C

main :: IO ()
main = $defaultMainGenerator

filtersNone :: IO (IOVector Filter)
filtersNone = do
  filters <- VM.new 1
  VM.write filters 0 $ Filter
    { filterId = vliUnknown
    , filterOptions = nullPtr
    }
  return filters

filtersOne :: IO (IOVector Filter)
filtersOne = do
  filters <- VM.new 2
  VM.write filters 0 $ Filter
    { filterId = undefined
    , filterOptions = nullPtr
    }

case_test1 :: Assertion
case_test1 = do
  block <- mallocForeignPtrBytes blockSize
  lzma_set_block_check block CheckNone
  do
    ret <- lzma_block_header_size block
    assertEqual "block_header_size 1" ret (Error ProgError)

  return ()
