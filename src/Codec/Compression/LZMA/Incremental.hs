module Codec.Compression.LZMA.Incremental
  (
  -- | This module provides incremental monadic compresssion and decompression
  -- interface.

  -- * Incremental compression and decompression
    I.DecompressStream
  , I.decompressIO
  , I.decompressST

  -- * Incremental decompression with random seek
  , I.DecompressParams(..)
  , I.defaultDecompressParams

  , I.SeekableDecompressStream
  , I.ReadRequest(..)
  , I.Compression(..)
  , I.Position
  , I.seekableDecompressIO

  , C.Index, I.StreamPadding
  , I.decodeIndicies
) where

import qualified Codec.Compression.LZMA.Internal as I
import qualified Codec.Compression.LZMA.Internal.C as C
