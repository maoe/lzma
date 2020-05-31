module Codec.Compression.LZMA
  (
  -- | This module provides pure functions for compressing and decompressing
  -- streams of data in the xz format and represented by lazy 'L.ByteString's.
  -- This makes it easy to use either in memory or with disk or network IO.

  -- * Simple compression and decompression
    compress
  , decompress

  -- * Extended API with control over (de)compression parameters

  -- ** Compression
  , compressWith
  , I.CompressParams
  , I.defaultCompressParams
  , I.compressPreset
  , I.compressIntegrityCheck
  , I.compressBufferSize
  , I.compressMemoryLimit
  , I.Preset
  , I.defaultPreset
  , I.extremePreset
  , I.customPreset
  , I.Check(..)

  -- ** Decompression
  , decompressWith
  , I.DecompressParams
  , I.defaultDecompressParams
  , I.decompressBufferSize
  , I.decompressMemoryLimit

  -- * Utils
  , isXzFile
  , isXzHandle
  ) where
import System.IO (Handle, IOMode(..), withFile)

import qualified Data.ByteString.Lazy as L

import qualified Codec.Compression.LZMA.Internal as I

-- | Comrpess a stream of data in the xz format
compress :: L.ByteString -> L.ByteString
compress = compressWith I.defaultCompressParams

-- | Like 'compress' but with the ability to specify vairous compression
-- parameters. Typical usage:
--
-- @
--  'compressWith' 'I.defaultCompressParams'
--    { 'I.compressPreset' = ...
--    , 'I.compressBufferSize' = ...
--    , 'I.compressMemoryLimit' = ...
--    , ...
--    }
-- @
compressWith :: I.CompressParams -> L.ByteString -> L.ByteString
compressWith = I.compress

-- | Decompress a stream of data in the xz format
decompress :: L.ByteString -> L.ByteString
decompress = decompressWith I.defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- @
--  'decompressWith' 'I.defaultDecompressParams'
--   { 'I.decompressBufferSize' = ...
--   , 'I.decompressMemoryLimit' = ...
--   }
-- @
decompressWith :: I.DecompressParams -> L.ByteString -> L.ByteString
decompressWith = I.decompress

isXzFile :: FilePath -> IO Bool
isXzFile path = withFile path ReadMode isXzHandle

isXzHandle :: Handle -> IO Bool
isXzHandle h = I.runDecodeStream h I.hasMagicBytes
