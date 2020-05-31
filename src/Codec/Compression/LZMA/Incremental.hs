module Codec.Compression.LZMA.Incremental
  (
  -- | This module provides incremental monadic compresssion and decompression
  -- interface.

  -- * Incremental compression and Decompression
  -- ** Compression streams
    I.CompressStream
  , I.compressIO
  , I.compressST

  -- ** Compression parameters
  , I.CompressParams
  , I.defaultCompressParams
  , I.compressPreset
  , I.compressIntegrityCheck
  , I.compressBufferSize
  , I.compressMemoryLimit

  -- ** Decompression streams
  , I.DecompressStream
  , I.decompressIO
  , I.decompressST

  -- ** Decompression parameters
  , I.DecompressParams
  , I.defaultDecompressParams
  , I.decompressBufferSize
  , I.decompressMemoryLimit

  -- * Incremental decompression with random seek
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
