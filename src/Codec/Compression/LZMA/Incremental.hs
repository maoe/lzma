module Codec.Compression.LZMA.Incremental
  (
  -- | This module provides incremental monadic compresssion and decompression
  -- interface.

  -- * Incremental compression and decompression
    I.CompressStream
  , I.compressIO
  , I.compressST

  , I.DecompressStream
  , I.decompressIO
  , I.decompressST

  -- * Incremental decompression with random seek
  , I.DecompressParams
  , I.defaultDecompressParams
  , I.decompressBufferSize
  , I.decompressMemoryLimit

  , I.SeekableDecompressStream
  , I.ReadRequest(..)
  , I.Compression(..)
  , I.Position
  , I.seekableDecompressIO

  , C.Index
  , I.DecodeStream
  , I.decodeIndex
  , I.decodeIndexIO
  , I.hasMagicBytes
) where

import qualified Codec.Compression.LZMA.Internal as I
import qualified Codec.Compression.LZMA.Internal.C as C
