module Codec.Compression.LZMA
  ( -- * Simple compression and decompression
    decompress

  -- * Extended API with control over compression parameters
  , decompressWith
  , DecompressParams(..)
  , defaultDecompressParams

  -- * Streaming interface for decompression
  , DecompressStream
  , DecompressError(..)
  , decompressST
  , decompressIO

  -- * Decompression with random seek support
  , Index.Index
  , Index.ReadRequest(..)
  , IndexedDecompressStream
  , indexedDecompressIO
  ) where
import qualified Data.ByteString.Lazy as L

import Codec.Compression.LZMA.Internal hiding (decompress)
import qualified Codec.Compression.LZMA.Index as Index
import qualified Codec.Compression.LZMA.Internal as Internal

-- | Decompress a stream of data in the xz format
decompress :: L.ByteString -> L.ByteString
decompress = decompressWith defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultCompressParams { ... }
--
decompressWith :: DecompressParams -> L.ByteString -> L.ByteString
decompressWith = Internal.decompress
