{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative

import Test.Tasty.QuickCheck
import Test.Tasty.TH
import qualified Data.ByteString.Lazy as L

import Codec.Compression.LZMA

main :: IO ()
main = $defaultMainGenerator

prop_roundTripIdentity :: L.ByteString -> Bool
prop_roundTripIdentity bytes = decompress (compress bytes) == bytes

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary
  shrink xs = L.pack <$> shrink (L.unpack xs)
