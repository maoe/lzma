{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codec.Compression.LZMA.Internal.Constants
  ( Ret(..), ErrorCode(..), toRet

  , Preset, fromPreset
  , defaultPreset, extremePreset, customPreset

  , Flags, fromFlags
  , tellNoCheckFlag, tellUnsupportedCheckFlag, tellAnyCheckFlag
  , concatenatedFlag

  , VLI
  , vliUnknown
  ) where

import Data.Bits
import Data.Semigroup as Sem
import Data.Word
import qualified GHC.Generics as GHC
import Prelude

#if DEBUG
import Text.Show.Pretty (PrettyVal)
#endif

#include <lzma.h>

-- | Return values used by several functions in liblzma.
--
-- Check the descriptions of specific functions to find out which return values they can return. With some functions the return values may have more specific meanings than described here; those differences are described per-function basis.
data Ret
  = Ok
  -- ^ Operation completed successfully.
  | StreamEnd
  -- ^ End of stream was reached.
  --
  -- In encoder, 'SyncFlush', 'FullFlush', or 'Finish' was finished. In
  -- decoder, this indicates that all the data was successfully decoded.
  --
  -- In all cases, when 'StreamEnd' is returned, the last output bytes
  -- should be picked from @'streamNextOut'@.
  | Error ErrorCode
  deriving (Show, Eq)

instance Enum Ret where
  fromEnum Ok = #{const LZMA_OK}
  fromEnum StreamEnd = #{const LZMA_STREAM_END}
  fromEnum (Error code) = fromEnum code

  toEnum #{const LZMA_OK} = Ok
  toEnum #{const LZMA_STREAM_END} = StreamEnd
  toEnum a = Error $ toEnum a

toRet :: Integral a => a -> Ret
toRet = toEnum . fromIntegral

data ErrorCode
  = NoCheck
  -- ^ Input stream has no integrity check.
  --
  -- This return value can be returned only if the 'tellNoCheckFlag' flag was
  -- used when initializing the decoder. 'NoCheck' is just a warning, and the
  -- decoding can be continued normally.
  --
  -- It is possible to call 'lzma_get_check' immediately after 'lzma_code' has
  -- returned 'NoCheck'. The result will naturally be 'CheckNone', but the
  -- possibility to call 'lzma_get_check' may be convenient in some
  -- applications.
  | UnsupportedCheck
  -- ^ Cannot calculate the integrity check.
  --
  -- The usage of this return value is different in encoders and decoders.
  --
  -- Encoders can return this value only from the initialization function. If
  -- initialization fails with this value, the encoding cannot be done,
  -- because there's no way to produce output with the correct integrity
  -- check.
  --
  -- Decoders can return this value only from 'lzma_code' and only if the
  -- 'tellUnsupportedCheckFlag' flag was used when initializing the decoder.
  -- The decoding can still be continued normally even if the check type is
  -- unsupported, but naturally the check will not be validated, and possible
  -- errors may go undetected.
  --
  -- With decoder, it is possible to call 'lzma_get_check' immediately after
  -- 'lzma_code' has returned 'UnsupportedCheck'. This way it is possible to
  -- find out what the unsupported Check ID was.
  | GetCheck
  -- ^ Integrity check type is now available.
  --
  -- This value can be returned only by the 'lzma_code' function and only if
  -- the decoder was initialized with the 'tellAnyCheckFlag' flag. 'GetCheck'
  -- tells the application that it may now call 'GetCheck' to find out the
  -- Check ID. This can be used, for example, to implement a decoder that
  -- accepts only files that have strong enough integrity check.
  | MemError
  -- ^ Cannot allocate memory.
  --
  -- Memory allocation failed, or the size of the allocation would be greater
  -- than SIZE_MAX.
  --
  -- Due to internal implementation reasons, the coding cannot be continued
  -- even if more memory were made available after 'MemError'.
  | MemlimitError
  -- ^ Memory usage limit was reached.
  --
  -- Decoder would need more memory than allowed by the specified memory
  -- usage limit. To continue decoding, the memory usage limit has to be
  -- increased with 'lzma_memlimit_set'. File format not recognized
  | FormatError
  -- ^ The decoder did not recognize the input as supported file format.
  --
  -- This error can occur, for example, when trying to decode .lzma format file
  -- with 'lzma_stream_decoder', because 'lzma_stream_decoder' accepts only the
  -- .xz format.
  | OptionsError
  -- ^ Invalid or unsupported options.
  --
  -- Invalid or unsupported options, for example
  --
  -- * unsupported filter(s) or filter options; or
  -- * reserved bits set in headers (decoder only).
  --
  -- Rebuilding liblzma with more features enabled, or upgrading to a newer
  -- version of liblzma may help.
  | DataError
  -- ^ Data is corrupt.
  --
  -- The usage of this return value is different in encoders and decoders. In
  -- both encoder and decoder, the coding cannot continue after this error.
  --
  -- Encoders return this if size limits of the target file format would be
  -- exceeded. These limits are huge, thus getting this error from an encoder
  -- is mostly theoretical. For example, the maximum compressed and
  -- uncompressed size of a .xz Stream is roughly 8 EiB (2^63 bytes).
  --
  -- Decoders return this error if the input data is corrupt. This can mean,
  -- for example, invalid CRC32 in headers or invalid check of uncompressed
  -- data.
  | BufError
  -- ^ No progress is possible.
  --
  -- This error code is returned when the coder cannot consume any new input
  -- and produce any new output. The most common reason for this error is
  -- that the input stream being decoded is truncated or corrupt.
  --
  -- This error is not fatal. Coding can be continued normally by providing
  -- more input and/or more output space, if possible.
  --
  -- Typically the first call to 'lzma_code' that can do no progress returns
  -- 'Ok' instead of 'BufError'. Only the second consecutive call doing no
  -- progress will return 'BufError'. This is intentional.
  --
  -- With zlib, @Z_BUF_ERROR@ may be returned even if the application is
  -- doing nothing wrong, so apps will need to handle @Z_BUF_ERROR@
  -- specially. The above hack guarantees that liblzma never returns
  -- 'BufError' to properly written applications unless the input file is
  -- truncated or corrupt. This should simplify the applications a little.
  | ProgError
  -- ^ Programming error.
  --
  -- This indicates that the arguments given to the function are invalid or
  -- the internal state of the decoder is corrupt.
  --
  -- * Function arguments are invalid or the structures pointed by the
  --   argument pointers are invalid e.g. if @'streamNextOut'@ has been set
  --   to NULL and @strm->avail_out > 0@ when calling 'lzma_code'.
  -- * @c_*@ functions have been called in wrong order e.g. 'lzma_code' was
  --   called right after 'lzma_end'.
  -- * If errors occur randomly, the reason might be flaky hardware.
  --
  -- If you think that your code is correct, this error code can be a sign of
  -- a bug in liblzma. See the documentation how to report bugs.
  deriving (Show, Eq)

instance Enum ErrorCode where
  fromEnum NoCheck = #{const LZMA_NO_CHECK}
  fromEnum UnsupportedCheck = #{const LZMA_UNSUPPORTED_CHECK}
  fromEnum GetCheck = #{const LZMA_GET_CHECK}
  fromEnum MemError = #{const LZMA_MEM_ERROR}
  fromEnum MemlimitError = #{const LZMA_MEMLIMIT_ERROR}
  fromEnum FormatError = #{const LZMA_FORMAT_ERROR}
  fromEnum OptionsError = #{const LZMA_OPTIONS_ERROR}
  fromEnum DataError = #{const LZMA_DATA_ERROR}
  fromEnum BufError = #{const LZMA_BUF_ERROR}
  fromEnum ProgError = #{const LZMA_PROG_ERROR}

  toEnum #{const LZMA_NO_CHECK} = NoCheck
  toEnum #{const LZMA_UNSUPPORTED_CHECK} = UnsupportedCheck
  toEnum #{const LZMA_GET_CHECK} = GetCheck
  toEnum #{const LZMA_MEM_ERROR} = MemError
  toEnum #{const LZMA_FORMAT_ERROR} = FormatError
  toEnum #{const LZMA_OPTIONS_ERROR} = OptionsError
  toEnum #{const LZMA_DATA_ERROR} = DataError
  toEnum #{const LZMA_BUF_ERROR} = BufError
  toEnum #{const LZMA_PROG_ERROR} = ProgError
  toEnum unmatched = error $ "ErrorCode.toEnum: Cannot match" ++ show unmatched

-- | Compression preset
newtype Preset = Preset Word32 deriving (Eq, Enum)

instance Sem.Semigroup Preset where
  Preset a <> Preset b = Preset $ a .|. b

instance Monoid Preset where
  mempty = customPreset 0
#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif

-- | Default compression preset.
--
-- It's not straightforward to recommend a default preset, because in some
-- cases keeping the resource usage relatively low is more important that
-- getting the maximum compression ratio.
defaultPreset :: Preset
defaultPreset = Preset (#const LZMA_PRESET_DEFAULT)

-- | Extreme compression preset.
--
-- This flag modifies the preset to make the encoding significantly slower
-- while improving the compression ratio only marginally. This is useful when
-- you don't mind wasting time to get as small result as possible.
--
-- This flag doesn't affect the memory usage requirements of the decoder (at
-- least not significantly). The memory usage of the encoder may be increased
-- a little but only at the lowest preset levels (0-3).
extremePreset :: Preset
extremePreset = Preset (#const LZMA_PRESET_EXTREME)

-- | Custom compression preset.
customPreset :: Word32 -> Preset
customPreset = Preset

fromPreset :: Integral a => Preset -> a
fromPreset = fromIntegral . fromEnum

newtype Flags = Flags Word32 deriving
  ( Eq, Enum, Bits, Show
#if !MIN_VERSION_base(4, 6, 0)
  , Num
#endif
  )

instance Sem.Semigroup Flags where
  Flags a <> Flags b = Flags $ a .|. b

instance Monoid Flags where
  mempty = Flags 0
#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif

fromFlags :: Integral a => Flags -> a
fromFlags = fromIntegral . fromEnum

-- | This flag makes 'code' return 'NoCheck' if the input stream being decoded
-- has no integrity check. Note that when used with 'autoDecoder', all .lzma
-- files will trigger NoCheck if 'tellNoCheckFlag' is used.
tellNoCheckFlag :: Flags
tellNoCheckFlag = Flags (#const LZMA_TELL_NO_CHECK)

-- | This flag makes 'code' return 'UnsupportedCheck' if the input stream has
-- an integrity check, but the type of the integrity check is not supported by
-- this liblzma version or build. Such files can still be decoded, but the
-- integrity check cannot be verified.
tellUnsupportedCheckFlag :: Flags
tellUnsupportedCheckFlag = Flags (#const LZMA_TELL_UNSUPPORTED_CHECK)

-- | This flag makes 'code' return 'GetCheck' as soon as the type of the
-- integrity check is known. The type can then be got with 'getCheck'.
tellAnyCheckFlag :: Flags
tellAnyCheckFlag = Flags (#const LZMA_TELL_ANY_CHECK)

-- | This flag enables decoding of concatenated files with file formats that
-- allow concatenating compressed files as is. From the formats currently
-- supported by liblzma, only the .xz format allows concatenated files.
-- Concatenated files are not allowed with the legacy .lzma format.
--
-- This flag also affects the usage of the `action' argument for 'code'. When
-- 'concatenatedFlag' is used, 'code' won't return 'StreamEnd' unless 'Finish'
-- is used as `action'. Thus, the application has to set 'Finish' in the same
-- way as it does when encoding.
--
-- If 'concatenatedFlag' is not used, the decoders still accept 'Finish' as
-- `action' for 'code', but the usage of 'Finish' isn't required.
concatenatedFlag :: Flags
concatenatedFlag = Flags (#const LZMA_CONCATENATED)

-- | Variable-length integer type
--
-- Valid VLI values are in the range [0, @LZMA_VLI_MAX@]. Unknown value is
-- indicated with 'LZMA_VLI_UNKNOWN', which is the maximum value of the
-- underlaying integer type.
newtype VLI = VLI (#type lzma_vli)
  deriving (Eq, Ord, Num, Real, Integral, Bits, GHC.Generic)

instance Show VLI where
  show (VLI n) = show n

instance Bounded VLI where
  minBound = VLI 0
  maxBound = VLI (#const LZMA_VLI_MAX)

instance Enum VLI where
  toEnum i
    | i < minBound =
      error $ "VLI.toEnum: VLI should be greater than " ++ show (minBound :: VLI)
    | i > maxBound =
      error $ "VLI.toEnum: VLI should be smaller than " ++ show (maxBound :: VLI)
    | otherwise = VLI $ toEnum i
  fromEnum (VLI n) = fromEnum n

vliUnknown :: VLI
vliUnknown = VLI (#const LZMA_VLI_UNKNOWN)

#if DEBUG
instance PrettyVal VLI
#endif
