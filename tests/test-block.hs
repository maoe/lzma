{-# LANGUAGE TemplateHaskell #-}
import Foreign
import Unsafe.Coerce (unsafeCoerce)

import Data.Vector.Storable.Mutable (IOVector)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Data.Vector.Storable.Mutable as VM

import Codec.Compression.LZMA.Internal.C

main :: IO ()
main = $defaultMainGenerator

filtersNone :: IO (IOVector Filter)
filtersNone = newFilters []

filtersOne :: IO (IOVector Filter)
filtersOne = do
  filters <- VM.new 2
  VM.write filters 0 Filter
    { filterId = undefined
    , filterOptions = nullPtr
    }
  return filters

case_test1 :: Assertion
case_test1 = do
  block <- newBlock
  lzma_set_block_check block CheckNone
  do
    ret <- lzma_block_header_size block
    assertEqual "block_header_size 1" ret (Error ProgError)

  return ()
