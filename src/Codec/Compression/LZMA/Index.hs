{-# LANGUAGE ViewPatterns #-}
module Codec.Compression.LZMA.Index
  ( -- * Index
    Index, StreamPadding
  , decodeIndicies
  -- * Iterator
  , IndexIter(..)
  , IndexIterStream(..)
  , IndexIterBlock(..)

  , ReadRequest(..)
  ) where
import System.IO

import Codec.Compression.LZMA.Internal.Index
import qualified Codec.Compression.LZMA.Internal.C as C

-- Decode indicies
decodeIndicies :: Handle -> IO (Index, StreamPadding)
decodeIndicies h = do
  size <- hFileSize h
  C.allocaStreamFlags $ \header ->
    C.allocaStreamFlags $ \footer -> do
      runSeekStream h $ indexDecodingToIO
        (parseIndicies (fromIntegral size))
        newDecoderState header footer
