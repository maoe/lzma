module Codec.Compression.LZMA
  (
  -- | This module provides pure functions for compressing and decompressing
  -- streams of data in the xz format and represented by lazy 'L.ByteString's.
  -- This makes it easy to use either in memory or with disk or network IO.

  -- * Simple compression and decompression
    decompress

  -- * Extended API with control over (de)compression parameters
  , decompressWith
  , I.DecompressParams
  , I.defaultDecompressParams
  , I.decompressBufferSize
  , I.decompressMemoryLimit
  ) where
import qualified Data.ByteString.Lazy as L

import qualified Codec.Compression.LZMA.Internal as I

-- | Decompress a stream of data in the xz format
decompress :: L.ByteString -> L.ByteString
decompress = decompressWith I.defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultCompressParams { ... }
--
decompressWith :: I.DecompressParams -> L.ByteString -> L.ByteString
decompressWith = I.decompress
