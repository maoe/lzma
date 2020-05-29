{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Codec.Compression.LZMA.Internal.C
  ( -- * Stream
    Stream
  , newStream
  , freeStream

  -- ** Getters and settters
  , streamAvailIn
  , streamAvailOut
  , streamNextIn
  , streamNextOut
  , streamTotalIn
  , streamTotalOut

  -- * Stream header and footer
  , StreamFlags
  , mallocStreamFlags
  , freeStreamFlags
  , streamFlagsCheck
  , streamFlagsVersion
  , streamFlagsBackwardSize
  , streamHeaderSize
  , streamHeaderDecode
  , streamFooterDecode
  , streamFlagsCompare

  -- * Block
  , Block
  , withBlock
  , newBlock
  , touchBlock
  , blockVersion
  , blockHeaderSize
  , blockCheck
  , blockFilters

  , blockHeaderSizeDecode
  , calculateBlockHeaderSize
  , blockHeaderEncode
  , blockHeaderDecode
  , blockCompressedSize
  , blockUnpaddedSize
  , blockTotalSize
  , blockEncoder
  , blockDecoder
  , blockBufferBound
  , blockBufferEncode
  -- , blockUncompEncode
  , blockBufferDecode

  -- * Filter
  , Filter(..)
  , BCJFilter(..)
  , newFilters
  , newFiltersMaxLength
  , touchFilters

  -- * Basic operations
  , Action(..)
  , code
  , end

  -- ** Encoders
  , easyEncoder
  -- ** Decoders
  , autoDecoder

  -- * Indexing
  -- ** Data types
  , Index(..)
  , withIndexFPtr
  , peekIndexFPtr

  -- ** Index manipulation
  , indexMemusage
  , indexMemused
  , indexInit
  , indexEnd
  , indexAppend
  , indexStreamFlags
  , indexChecks
  , indexStreamPadding
  , indexStreamCount
  , indexBlockCount
  , indexSize
  , indexStreamSize
  , indexTotalSize
  , indexFileSize
  , indexUncompressedSize
  , indexCat
  , indexDup
  , indexEncoder
  , indexDecoder
  -- , indexBufferEncode
  -- , indexBufferDecode
  -- ** Iterator manipulation
  , IndexIter
  , withIndexIter
  , indexIterStreamFlags
  , indexIterStreamNumber
  , indexIterStreamBlockCount
  , indexIterStreamCompressedOffset
  , indexIterStreamUncompressedOffset
  , indexIterStreamCompressedSize
  , indexIterStreamUncompressedSize
  , indexIterStreamPadding
  , indexIterBlockNumberInFile
  , indexIterBlockCompressedFileOffset
  , indexIterBlockUncompressedFileOffset
  , indexIterBlockNumberInStream
  , indexIterBlockCompressedStreamOffset
  , indexIterBlockUncompressedStreamOffset
  , indexIterBlockUncompressedSize
  , indexIterBlockUnpaddedSize
  , indexIterBlockTotalSize
  , IndexIterMode(..)

  , indexIterInit
  , indexIterRewind
  , indexIterNext
  , indexIterLocate

  -- * Others
  -- ** Return types
  , Ret(..), ErrorCode(..)
  , Check(..)
  -- ** Preset
  , Preset
  , defaultPreset, extremePreset, customPreset
  -- ** Flags
  , Flags
  , tellNoCheckFlag, tellUnsupportedCheckFlag, tellAnyCheckFlag
  , concatenatedFlag
  -- ** Variable-length integer
  , VLI
  , vliUnknown
  ) where
import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Data.List (unfoldr)
import Foreign
import Foreign.C
import Prelude

import Data.StateVar hiding (get)
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VM

import Codec.Compression.LZMA.Internal.Constants

#if DEBUG
import Debug.Trace
import Text.Show.Pretty (PrettyVal(..))
import qualified Text.Show.Pretty as Pretty
#endif

#include <lzma.h>

{# context prefix="lzma" #}

------------------------------------------------------------
-- Low-level interface to liblzma

-- | Passing data to and from liblzma.
--
-- The 'Stream' structure is used for:
--
--   * passing pointers to input and output buffers to liblzma;
--   * defining custom memory hander functions; and
--   * holding a pointer to coder-specific internal data structures.
--
-- Typical usage:
--
--  * Use 'newStream' to allocate a 'Stream'.
--  * Initialize a coder to the stream, for example by using
--  'easyEncoder' or 'autoDecoder'. Some notes:
--
--      * In contrast to zlib, 'streamNextIn' and 'streamNextOut' are ignored by
--        all initialization functions, thus it is safe to not initialize them
--        yet.
--      * The initialization functions always set 'streamTotalIn' and
--        'streamTotalOut' to zero.
--      * If the initialization function fails, no memory is left allocated
--        that would require freeing with 'end' even if some memory was
--        associated with the 'Stream' structure when the initialization
--        function was called.
--
--   * Use 'code' to do the actual work.
--   * Once the coding has been finished, the existing 'Stream' can be
--     reused. It is OK to reuse 'Stream' with different initialization
--     function without calling 'end' first. Old allocations are
--     automatically freed.
--   * Finally, use 'end' to free the allocated memory. 'end' never
--     frees the 'Stream' structure itself.
--
-- Application may modify the values of 'streamTotalIn' and 'streamTotalOut' as
-- it wants. They are updated by liblzma to match the amount of data read and
-- written but aren't used for anything else except as a possible return values
-- from lzma_get_progress().
--
-- NOTE: 'Stream' objects aren't going to be GC'ed automatically. The user needs
-- to call 'freeStream' at the end of the 'Stream''s life cycle.
{# pointer *lzma_stream as Stream newtype #}

deriving instance Show Stream

foreign import ccall "lzma.h lzma_init"
  c_lzma_init :: Ptr Stream -> IO ()

foreign import ccall "lzma.h lzma_end"
  c_lzma_end :: Ptr Stream -> IO ()

-- | Allocate a new stream.
newStream :: IO Stream
newStream = do
  p <- mallocBytes {# sizeof lzma_stream #}
  c_lzma_init p
  return $ Stream p

freeStream :: Stream -> IO ()
freeStream (Stream p) = c_lzma_end p

-- | Number of available input bytes in the input buffer.
streamAvailIn :: Stream -> StateVar Int
streamAvailIn stream = StateVar get set
  where
    get = fromIntegral <$> {# get lzma_stream.avail_in #} stream
    set = {# set lzma_stream.avail_in #} stream . fromIntegral

-- | Amount of free space in the output buffer.
streamAvailOut :: Stream -> StateVar Int
streamAvailOut stream = StateVar get set
  where
    get = fromIntegral <$> {# get lzma_stream.avail_out #} stream
    set = {# set lzma_stream.avail_out #} stream . fromIntegral

-- | Pointer to the next input byte.
streamNextIn :: Stream -> StateVar (Ptr Word8)
streamNextIn stream = StateVar get set
  where
    get = castPtr <$> {# get lzma_stream.next_in #} stream
    set = {# set lzma_stream.next_in #} stream . castPtr

-- | Pointer to the next output position.
streamNextOut :: Stream -> StateVar (Ptr Word8)
streamNextOut stream = StateVar get set
  where
    get = castPtr <$> {# get lzma_stream.next_out #} stream
    set = {# set lzma_stream.next_out #} stream . castPtr

-- | Total number of bytes read by liblzma
streamTotalIn :: Stream -> StateVar Int
streamTotalIn stream = StateVar get set
  where
    get = fromIntegral <$> {# get lzma_stream.total_in #} stream
    set = {# set lzma_stream.total_in #} stream . fromIntegral

-- | Total number of bytes written by liblzma
streamTotalOut :: Stream -> StateVar Int
streamTotalOut stream = StateVar get set
  where
    get = fromIntegral <$> {# get lzma_stream.total_out #} stream
    set = {# set lzma_stream.total_out #} stream . fromIntegral

-- | The action argument for 'code'.
--
-- After the first use of 'SyncFlush', 'FullFlush', or 'Finish', the same
-- 'Action' must be used until 'code' returns 'StreamEnd'. Also, the
-- amount of input (that is, 'streamAvailIn') must not be modified by the
-- application until 'code' returns 'StreamEnd'. Changing the 'Action' or
-- modifying the amount of input will make 'code' return 'ProgError'.
{# enum lzma_action as Action
  { LZMA_RUN as Run
  -- ^ Continue coding.
  --
  -- Encoder: Encode as much input as possible. Some internal buffering will
  -- probably be done (depends on the filter chain in use), which causes
  -- latency: the input used won't usually be decodeable from the output of
  -- the same 'code' call.
  --
  -- Decoder: Decode as much input as possible and produce as much output as
  -- possible.
  , LZMA_SYNC_FLUSH as SyncFlush
  -- ^ Make all the input available at output.
  --
  -- Normally the encoder introduces some latency. LZMA_SYNC_FLUSH forces all
  -- the buffered data to be available at output without resetting the
  -- internal state of the encoder. This way it is possible to use compressed
  -- stream for example for communication over network.
  --
  -- Only some filters support 'SyncFlush'. Trying to use 'SyncFlush' with
  -- filters that don't support it will make 'code' return 'OptionsError'.
  -- For example, LZMA1 doesn't support 'SyncFlush' but LZMA2 does.
  --
  -- Using 'SyncFlush' very often can dramatically reduce the compression
  -- ratio. With some filters (for example, LZMA2), fine-tuning the
  -- compression options may help mitigate this problem significantly (for
  -- example, match finder with LZMA2).
  --
  -- Decoders don't support 'SyncFlush'.
  , LZMA_FULL_FLUSH as FullFlush
  -- ^ Finish encoding of the current Block.
  --
  -- All the input data going to the current Block must have been given to the
  -- encoder (the last bytes can still be pending in* next_in). Call 'code'
  -- with 'FullFlush' until it returns 'StreamEnd'. Then continue normally
  -- with 'Run' or finish the Stream with 'Finish.
  --
  -- This action is currently supported only by Stream encoder and easy
  -- encoder (which uses Stream encoder). If there is no unfinished Block, no
  -- empty Block is created.
  , LZMA_FINISH as Finish
  -- ^ Finish the coding operation.
  --
  -- All the input data must have been given to the encoder (the last bytes
  -- can still be pending in next_in). Call 'code' with 'Finish' until it
  -- returns 'StreamEnd'. Once 'Finish' has been used, the amount of input
  -- must no longer be changed by the application.
  --
  -- When decoding, using 'Finish' is optional unless the 'concatenatedFlag'
  -- was used when the decoder was initialized. When 'concatenatedFlag' was
  -- not used, the only effect of 'Finish' is that the amount of input must
  -- not be changed just like in the encoder.
  -- , LZMA_FULL_BARRIER as FullBarrier
  } deriving (Show) #}

fromAction :: Integral a => Action -> a
fromAction = fromIntegral . fromEnum

-- | Type of the integrity check (Check ID)
--
-- The .xz format supports multiple types of checks that are calculated from
-- the uncompressed data. They vary in both speed and ability to detect
-- errors.
{# enum lzma_check as Check
  { LZMA_CHECK_NONE as CheckNone
  -- ^ No Check is calculated.
  --
  -- Size of the Check field: 0 bytes
  , LZMA_CHECK_CRC32 as CheckCrc32
  -- ^ CRC32 using the polynomial from the IEEE 802.3 standard
  --
  -- Size of the Check field: 4 bytes
  , LZMA_CHECK_CRC64 as CheckCrc64
  -- ^ CRC64 using the polynomial from the ECMA-182 standard
  --
  -- Size of the Check field: 8 bytes
  , LZMA_CHECK_SHA256 as CheckSha256
  -- ^ SHA-256
  --
  -- Size of the Check field: 32 bytes
  } deriving (Show, Bounded, Ord, Eq) #}

fromCheck :: Integral a => Check -> a
fromCheck = fromIntegral . fromEnum

-- | Type of reserved enumeration variable in structures.
--
-- To avoid breaking library ABI when new fqeatures are added, several
-- structures contain extra variables that may be used in future. Since
-- sizeof(enum) can be different than sizeof(int), and sizeof(enum) may even
-- vary depending on the range of enumeration constants, we specify a separate
-- type to be used for reserved enumeration variables. All enumeration
-- constants in liblzma API will be non-negative and less than 128, which
-- should guarantee that the ABI won't break even when new constants are added
-- to existing enumerations.
{# enum lzma_reserved_enum as ReservedEnum {underscoreToCase} #}

-- | Initialize .xz Stream encoder using a preset number.
--
-- This function is intended for those who just want to use the basic features
-- if liblzma (that is, most developers out there).
--
-- Returns:
--
--   * 'Ok': Initialization succeeded. Use 'code' to encode your data.
--   * 'MemError': Memory allocation failed.
--   * 'OptionsError': The given compression preset is not supported by this
--      build of liblzma.
--   * 'UnsupportedCheck': The given check type is not supported by this
--      liblzma build.
--   * 'ProgError': One or more of the parameters have values that will never
--      be valid. For example, strm == NULL.
--
-- If initialization fails (return value is not 'Ok'), all the memory
-- allocated for *strm by liblzma is always freed. Thus, there is no need to
-- call 'end' after failed initialization.
--
-- If initialization succeeds, use 'code' to do the actual encoding. Valid
-- values for 'Action' (the second argument of 'code') are 'Run',
-- 'SyncFlush', 'FullFlush', and 'Finish'. In future, there may be compression
-- levels or flags that don't support 'SyncFlush'.
{# fun easy_encoder as ^
  { `Stream'
  -- ^ 'Stream' that is at least initialized with LZMA_STREAM_INIT.
  , fromPreset `Preset'
  -- ^ Compression preset to use. A preset consist of level number and zero or
  -- more flags. Usually flags aren't used, so preset is simply a number
  -- [0, 9] which match the options -0 ... -9 of the xz command line tool.
  -- Additional flags can be be set using bitwise-or with the preset level
  -- number, e.g. 6 | LZMA_PRESET_EXTREME.
  , fromCheck `Check'
  -- ^ Integrity check type to use. See check.h for available checks. The xz
  -- command line tool defaults to 'CheckCrc64', which is a good choice if you
  -- are unsure. 'CheckCrc32' is good too as long as the uncompressed file is
  -- not many gigabytes.
  } -> `Ret' toRet
  #}

-- | Decode .xz Streams and .lzma files with autodetection.
--
-- This decoder autodetects between the .xz and .lzma file formats, and calls
-- 'lzma_stream_decoder' or 'lzma_alone_decoder' once the type of the input
-- file has been detected.
--
-- Returns:
--
--   * 'Ok': Initialization was successful.
--   * 'MemError': Cannot allocate memory.
--   * 'OptionsError': Unsupported flags
--   * 'ProgError'
{# fun auto_decoder as ^
  { `Stream'
  -- ^ Properly prepared stream
  , `Word64'
  -- ^ Memory usage limit as bytes. Use 'maxBound' to effectively disable the
  -- limiter.
  , fromFlags `Flags'
  -- ^ 'mappend' of flags, or 'mzero' for no flags.
  } -> `Ret' toRet
  #}

-- | Encode or decode data.
--
-- Once the 'Stream' has been successfully initialized (e.g. with
-- 'streamEncoder'), the actual encoding or decoding is done using this
-- function. The application has to update 'streamNextIn', 'streamAvailIn',
-- 'streamNextOut', and 'streamAvailOut' to pass input to and get output from
-- liblzma.
--
-- See the description of the coder-specific initialization function to find
-- out what `Action' values are supported by the coder.
{# fun code as ^
  { `Stream'
  , fromAction `Action'
  } -> `Ret' toRet
  #}

-- | Free memory allocated for the coder data structures.
--
-- After @'end' strm@, strm->internal is guaranteed to be @NULL@. No other
-- members of the 'Stream' structure are touched.
--
-- Note: zlib indicates an error if application 'end's unfinished stream
-- structure. liblzma doesn't do this, and assumes that application knows
-- what it is doing.
{# fun end as ^
  { `Stream'
  -- ^ Stream that is at least initialized with LZMA_STREAM_INIT
  } -> `()' #}

------------------------------------------------------
-- Valiable-length integer handling


------------------------------------------------------
-- Stream headers and footers

-- | Options for encoding/decoding Stream Header and Stream Footer.
{# pointer *lzma_stream_flags as StreamFlags newtype #}

deriving instance Storable StreamFlags
deriving instance Show StreamFlags

mallocStreamFlags :: IO StreamFlags
mallocStreamFlags = StreamFlags <$> malloc

freeStreamFlags :: StreamFlags -> IO ()
freeStreamFlags (StreamFlags ptr) = free ptr

streamFlagsCheck :: StreamFlags -> GettableStateVar Check
streamFlagsCheck flags = do
  n <- {# get lzma_stream_flags.check #} flags
  return $ toEnum $ fromIntegral n

streamFlagsVersion :: StreamFlags -> GettableStateVar Word32
streamFlagsVersion flags = do
  version <- {# get lzma_stream_flags.version #} flags
  return $ fromIntegral version

-- | Backward Size.
--
-- Backward Size must be a multiple of four bytes. In this Stream format
-- version, Backward Size is the size of the Index field.
--
-- Backward Size isn't actually part of the Stream Flags field, but it is
-- convenient to include in this structure anyway. Backward Size is present
-- only in the Stream Footer. There is no need to initialize
-- 'streamFlagsBackwardSize' when encoding Stream Header.
--
-- 'streamHeaderDecode' always sets 'streamFlagsBackwardSize' to
-- @LZMA_VLI_UNKNOWN@ so that it is convenient to use 'streamFlagsCompare' when
-- both Stream Header and Stream Footer have been decoded.

streamFlagsBackwardSize :: StreamFlags -> GettableStateVar VLI
streamFlagsBackwardSize flags =
  fromIntegral <$> {# get lzma_stream_flags.backward_size #} flags

-- | Size of Stream Header and Stream Footer.
--
-- Stream Header and Stream Footer have the same size and they are not going to
-- change even if a newer version of the .xz file format is developed in future.
streamHeaderSize :: VLI
streamHeaderSize = {# const LZMA_STREAM_HEADER_SIZE #}

-- | Decode Stream Header.
--
-- 'streamFlagsBackwardSize' is always set to @LZMA_VLI_UNKNOWN@. This is to
-- help comparing Stream Flags from Stream Header and Stream Footer with
-- 'streamFlagsCompare'.
--
-- Returns
--
--  * 'Ok': Decoding was successful.
--  * 'FormatError': Magic bytes don't match, thus the given buffer cannot be
--     Stream Header.
--  * 'DataError': CRC32 doesn't match, thus the Stream Header is corrupt.
--  * 'OptionsError': Unsupported options are present in the header.
--
-- Note: When decoding .xz files that contain multiple Streams, it may make
-- sense to print "file format not recognized" only if decoding of the Stream
-- Header of the first Stream gives 'FormatError'. If non-first Stream Header
-- gives 'FormatError', the message used for 'DataError' is probably more
-- appropriate.
--
-- For example, Stream decoder in liblzma uses 'DataError' if 'FormatError' is
-- returned by 'streamHeaderDecode' when decoding non-first Stream.
{# fun stream_header_decode as ^
  { `StreamFlags'
  -- ^ Target for the decoded Stream Header options.
  , castPtr `Ptr Word8'
  -- ^ Beginning of the input buffer of @LZMA_STREAM_HEADER_SIZE@ bytes.
  } -> `Ret' toRet
  #}

-- | Decode Stream Footer.
--
-- Returns
--
--  * 'Ok': Decoding was successful.
--  * 'FormatError': Magic bytes don't match, thus the given buffer cannot be
--     Stream Footer.
--  * 'DataError': CRC32 doesn't match, thus the Stream Footer is corrupt.
--  * 'OptionsError': Unsupported options are present in Stream Footer.
--
-- Note: If Stream Header was already decoded successfully, but decoding
-- Stream Footer returns 'FormatError', the application should probably
-- report some other error message than "file format not recognized", since
-- the file more likely is corrupt (possibly truncated). Stream decoder in
-- liblzma uses 'DataError' in this situation.
{# fun stream_footer_decode as ^
  { `StreamFlags'
  -- ^ Target for the decoded Stream Header options.
  , castPtr `Ptr Word8'
  -- ^ Beginning of the input buffer of @LZMA_STREAM_HEADER_SIZE@ bytes.
  } -> `Ret' toRet
  #}

-- | Compare two 'StreamFlags' structures.
--
-- 'streamFlagsBackwardSize' values are compared only if both are not
-- @LZMA_VLI_UNKNOWN@.
--
-- Returns
--
--  * 'Ok': Both are equal. If either had 'streamFlagsBackwardSize' set to
--    @LZMA_VLI_UNKNOWN@, 'streamFlagsBackwardSize' values were not compared or
--    validated.
--  * 'DataError': The structures differ.
--  * 'OptionsError': version in either structure is greater than the maximum
--    supported version (currently zero).
--  * 'ProgError': Invalid value, e.g. invalid check or
--    'streamFlagsBackwardSize'.
{# fun stream_flags_compare as ^
  { `StreamFlags'
  , `StreamFlags'
  } -> `Ret' toRet
  #}

------------------------------------------------------
-- Blocks

{# pointer *lzma_block as Block foreign newtype #}

newBlock :: IO Block
newBlock = Block <$> mallocForeignPtrBytes {# sizeof lzma_block #}

touchBlock :: Block -> IO ()
touchBlock (Block blockFPtr) = touchForeignPtr blockFPtr

-- | Block format version.
--
-- To prevent API and ABI breakages if new features are needed in the 'Block'
-- field, a version number is used to indicate which fields in this structure
-- are in use. For now, version must always be zero. With non-zero version,
-- most 'Block' related functions will return 'OptionsError'.
--
-- Read by: All functions that take pointer to 'Block' as argument, including
-- 'blockHeaderDecode'.
--
-- Written by: 'blockHeaderDecode'
blockVersion :: Block -> SettableStateVar Word32
blockVersion block = SettableStateVar set
  where
    set version =
      withBlock block $ \blockPtr ->
        {# set lzma_block.version #} blockPtr (fromIntegral version)

-- | Size of the Block Header field.
--
-- This is always a multiple of four.
--
-- Read by:
--
--    * 'blockHeaderEncode'
--    * 'blockHeaderDecode'
---   * 'blockCompressedSize'
--    * 'blockUnpaddedSize'
--    * 'blockTotalSize'
--    * 'blockDecoder'
--    * 'blockBufferDecode'
--
-- Written by:
--
--    * 'blockHeaderSize'
--    * 'blockBufferEncode'
blockHeaderSize :: Block -> StateVar Word32
blockHeaderSize block = StateVar get set
  where
    get =
      withBlock block $ \blockPtr ->
        fromIntegral <$> {# get lzma_block.header_size #} blockPtr
    set size =
      withBlock block $ \blockPtr ->
        {# set lzma_block.header_size #} blockPtr (fromIntegral size)

-- | Type of integrity Check.
--
-- The Check ID is not stored into the Block Header, thus its value must be
-- provided also when decoding.
--
-- Read by:
--
--    * 'blockHeaderEncode'
--    * 'blockHeaderDecode'
--    * 'blockCompressedSize'
--    * 'blockUnpaddedSize'
--    * 'blockTotalSize'
--    * 'blockEncoder'
--    * 'blockDecoder'
--    * 'blockBufferEncode'
--    * 'blockBufferDecode'
blockCheck :: Block -> SettableStateVar Check
blockCheck block = SettableStateVar set
  where
    set check =
      withBlock block $ \blockPtr ->
        {# set lzma_block.check #} blockPtr (fromIntegral (fromEnum check))

-- | Array of filters.
--
-- There can be 1-4 filters. The end of the array is marked with
-- @.id = LZMA_VLI_UNKNOWN@.
--
-- Read by:
--
--     * 'blockHeaderSize'
--     * 'blockHeaderEncode'
--     * 'blockEncoder'
--     * 'blockDecoder'
--     * 'blockBufferEncode'
--     * 'blockBufferDecode'
--
-- Written by:
--
--     * 'blockHeaderDecode': Note that this does NOT free() the old
--       filter options structures. All unused filters[] will have
--       @.id == LZMA_VLI_UNKNOWN@ and .options == NULL. If decoding fails, all
--       filters[] are guaranteed to be LZMA_VLI_UNKNOWN and NULL.
--
-- Note: Because of the array is terminated with @.id = LZMA_VLI_UNKNOWN@, the
-- actual array must have LZMA_FILTERS_MAX + 1 members or the Block Header
-- decoder will overflow the buffer.
blockFilters :: Block -> SettableStateVar (IOVector Filter)
blockFilters block = SettableStateVar set
  where
    set (VM.MVector _ filtersFPtr) =
      withBlock block $ \blockPtr ->
        withForeignPtr filtersFPtr $ \filtersPtr ->
          {# set lzma_block.filters #} blockPtr (castPtr filtersPtr)

blockHeaderSizeDecode :: Word8 -> Word32
blockHeaderSizeDecode =
  fromIntegral .
  lzma_block_header_size_decode .
  fromIntegral

foreign import capi "lzma.h lzma_block_header_size_decode"
  lzma_block_header_size_decode :: CUChar -> CUInt

-- | Calculate Block Header Size.
--
-- Calculate the minimum size needed for the Block Header field using the
-- settings specified in the 'Block' structure. Note that it is OK to
-- increase the calculated 'blockHeaderSize' value as long as it is a multiple
-- of four and doesn't exceed LZMA_BLOCK_HEADER_SIZE_MAX. Increasing
-- 'blockHeaderSize' just means that 'blockHeaderEncode' will add Header
-- Padding.
--
-- Returns
--
--    * 'Ok': Size calculated successfully and stored to 'blockHeaderSize'.
--    * 'OptionsError': Unsupported version, filters or filter options.
--    * 'ProgError': Invalid values like compressed_size == 0.
--
-- Note: This doesn't check that all the options are valid i.e. this may return
-- 'Ok' even if 'blockHeaderEncode' or 'blockEncoder' would fail.
-- If you want to validate the filter chain, consider using
-- 'lzma_memlimit_encoder' which as a side-effect validates the filter chain.
{# fun block_header_size as calculateBlockHeaderSize
  { `Block'
  } -> `Ret' toRet #}

{# fun block_header_encode as ^
  { `Block'
  , castPtr `Ptr Word8'
  } -> `Ret' toRet
  #}

{# fun block_header_decode as ^
  { `Block'
  , passNullPtr- `Ptr ()'
  , castPtr `Ptr Word8'
  } -> `Ret' toRet
  #}

-- | Validate and set Compressed Size according to Unpadded Size.
--
-- Block Header stores Compressed Size, but Index has Unpadded Size. If the
-- application has already parsed the Index and is now decoding Blocks, it can
-- calculate Compressed Size from Unpadded Size. This function does exactly
-- that with error checking:
--
--    * Compressed Size calculated from Unpadded Size must be positive integer,
--      that is, Unpadded Size must be big enough that after Block Header and
--      Check fields there's still at least one byte for Compressed Size.
--    * If Compressed Size was present in Block Header, the new value
--      calculated from Unpadded Size is compared against the value from Block
--      Header.
--
--  Note: This function must be called after decoding the Block Header field so
-- that it can properly validate Compressed Size if it was present in Block
-- Header.
--
-- Returns
--
--    * 'Ok': 'blockCompressedSize' was set successfully.
--    * 'DataError': unpadded_size is too small compared to 'blockHeaderSize'
--       and lzma_check_size('blockCheck').
--    * 'ProgError': Some values are invalid. For example, 'blockHeaderSize'
--       must be a multiple of four and between 8 and 1024 inclusive.
{# fun block_compressed_size as ^
  { `Block'
  , fromIntegral `VLI'
  -- ^ Unpadded size.
  } -> `Ret' toRet
  #}


{# fun block_unpadded_size as ^
  { `Block'
  } -> `VLI' fromIntegral
  #}

{# fun block_total_size as ^
  { `Block'
  } -> `VLI' fromIntegral
  #}

{# fun block_encoder as ^
  { `Stream'
  , `Block'
  } -> `Ret' toRet
  #}

-- | Initialize .xz Block decoder.
--
-- Valid actions for 'code' are 'Run' and 'Finish'. Using 'Finish' is not
-- required. It is supported only for convenience.
--
-- Returns
--
--    * 'Ok': All good, continue with 'code'.
--    * 'UnsupportedCheck': Initialization was successful, but the given Check
--      ID is not supported, thus Check will be ignored.
--    * 'ProgError'
--    * 'MemError'
{# fun block_decoder as ^
  { `Stream'
  , `Block'
  } -> `Ret' toRet
  #}

{# fun block_buffer_bound as ^
  { fromIntegral `Word64'
  } -> `Word64' fromIntegral
  #}

{# fun block_buffer_encode as ^
  { `Block'
  , passNullPtr- `Ptr ()'
  , castPtr `Ptr Word8'
  , fromIntegral `Word64'
  , castPtr `Ptr Word8'
  , castPtr `Ptr Word64'
  , fromIntegral `Word64'
  } -> `Ret' toRet
  #}

-- {# fun block_uncomp_encode as ^
--   { `Block'
--   , castPtr `Ptr Word8'
--   , fromIntegral `Word64'
--   , castPtr `Ptr Word8'
--   , castPtr `Ptr Word64'
--   , fromIntegral `Word64'
--   } -> `Ret' toRet
--   #}

-- | Single-call .xz Block decoder.
--
-- This is single-call equivalent of 'blockDecoder', and requires that
-- the caller has already decoded Block Header and checked its memory usage.
--
-- Returns
--
--    * 'Ok': Decoding was successful.
--    * 'OptionsError'
--    * 'DataError'
--    * 'MemError'
--    * 'BufError': Output buffer was too small.
--    * 'ProgError'
{# fun block_buffer_decode as ^
  { `Block'
  -- ^ Block options just like with 'blockDecoder'.
  , passNullPtr- `Ptr ()'
  -- ^ 'Allocator' for custom allocator functions. Set to 'Nothing' to use
  -- @malloc()@ and @free()@.
  , castPtr `Ptr Word8'
  -- ^ Beginning of the input buffer
  , castPtr `Ptr Word64'
  -- ^ The next byte will be read from in[*in_pos]. *in_pos is updated only if
  -- decoding succeeds.
  , fromIntegral `Word64'
  -- ^ Size of the input buffer; the first byte that won't be read is
  -- @in[in_size]@.
  , castPtr `Ptr Word8'
  -- ^ Beginning of the output buffer
  , castPtr `Ptr Word64'
  -- ^ The next byte will be written to @out[*out_pos]@. *out_pos is updated
  -- only if encoding succeeds.
  , fromIntegral `Word64'
  -- ^ Size of the out buffer; the first byte into which no data is written to
  -- is out[out_size].
  } -> `Ret' toRet
  #}

------------------------------------------------------
-- Filters

{# enum define BCJFilter
  { LZMA_FILTER_X86 as FilterX86
  -- ^ Filter for x86 binaries
  , LZMA_FILTER_POWERPC as FilterPowerpc
  -- ^ Filter for Big endian PowerPC binaries
  , LZMA_FILTER_IA64 as FilterIa64
  -- ^ Filter for IA-64 (Itanium) binaries.
  , LZMA_FILTER_ARM as FilterArm
  -- ^ Filter for ARM binaries.
  , LZMA_FILTER_ARMTHUMB as FilterArmthumb
  -- ^ Filter for ARM-Thumb binaries.
  , LZMA_FILTER_SPARC as FilterSparc
  -- ^ Filter for SPARC binaries.
  } deriving (Show, Eq)
  #}

data Filter = Filter
  { filterId :: BCJFilter
  , filterOptions :: Ptr ()
  }

instance Storable Filter where
  sizeOf _ = {# sizeof lzma_filter #}
  alignment _ = {# alignof lzma_filter #}
  peek p = Filter
    <$> fmap (toEnum . fromIntegral) ({# get lzma_filter.id #} p)
    <*> {# get lzma_filter.options #} p
  poke p Filter {..} = do
    {# set lzma_filter.id #} p (fromIntegral $ fromEnum filterId)
    {# set lzma_filter.options #} p filterOptions

filtersMax :: Int
filtersMax = {# const LZMA_FILTERS_MAX #}

newFilters :: [Filter] -> IO (IOVector Filter)
newFilters filters = do
  fptr <- mallocForeignPtrArray0 len
  withForeignPtr fptr $ \p -> do
    p' <- foldM pokeThenAdvance p filters
    -- set the terminal element at the end of the array
    assert (p' `minusPtr` p == len * sizeOf (undefined :: Filter)) $ return ()
    {# set lzma_filter.id #} p' (fromIntegral vliUnknown)
    {# set lzma_filter.options #} p' nullPtr
  return $ VM.MVector len fptr
  where
    len = length filters
    pokeThenAdvance :: Storable a => Ptr a -> a -> IO (Ptr a)
    pokeThenAdvance p a = do
      poke p a
      return $ p `advancePtr` 1

newFiltersMaxLength :: IO (IOVector Filter)
newFiltersMaxLength = VM.new (filtersMax + 1)

touchFilters :: IOVector Filter -> IO ()
touchFilters (VM.MVector _ fptr) = touchForeignPtr fptr

------------------------------------------------------
-- Indexing

-- | Opaque data type to hold the Index(es) and other information.
--
-- 'Index' often holds just one .xz Index and possibly the Stream Flags of
-- the same Stream and size of the Stream Padding field. However, multiple
-- 'Index'es can be concatenated with 'indexCat' and then there may be
-- information about multiple Streams in the same 'Index'.
--
-- Notes about thread safety: Only one thread may modify 'Index' at a time.
-- All functions that take non-const pointer to 'Index' modify it. As long
-- as no thread is modifying the 'Index', getting information from the same
-- 'Index' can be done from multiple threads at the same time with functions
-- that take a const pointer to 'Index' or use 'IndexIter'. The same
-- iterator must be used only by one thread at a time, of course, but there can
-- be as many iterators for the same 'Index' as needed.
{# pointer *lzma_index as Index newtype #}

deriving instance Show Index
deriving instance Storable Index

-- | Dereference a @'ForeignPtr' 'Index'@ then apply a function to the 'Index'.
withIndexFPtr :: ForeignPtr Index -> (Index -> IO a) -> IO a
withIndexFPtr fptr f = withForeignPtr fptr (peek >=> f)

-- | Pointer to an 'IndexIter'.
{# pointer *lzma_index_iter as IndexIter foreign newtype #}

deriving instance Show IndexIter

peekIndexFPtr :: ForeignPtr Index -> IO Index
peekIndexFPtr fptr = withIndexFPtr fptr return

indexIterStreamFlags :: IndexIter -> GettableStateVar StreamFlags
indexIterStreamFlags iter = get
  where
    get = withIndexIter iter {# get lzma_index_iter.stream.flags #}

-- | Stream number in the 'Index'.
--
-- The first Stream is 1.
indexIterStreamNumber :: IndexIter -> GettableStateVar VLI
indexIterStreamNumber iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.stream.number #}

-- | Number of Blocks in the Stream.
--
-- If this is zero, the block structure below has undefined values.
indexIterStreamBlockCount :: IndexIter -> GettableStateVar VLI
indexIterStreamBlockCount iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.stream.block_count #}

-- | Compressed start offset of this Stream.
--
-- The offset is relative to the beginning of the 'Index' (i.e. usually the
-- beginning of the .xz file).
indexIterStreamCompressedOffset :: IndexIter -> GettableStateVar VLI
indexIterStreamCompressedOffset iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.stream.compressed_offset #}

-- | Uncompressed start offset of this Stream.
--
-- The offset is relative to the beginning of the 'Index' (i.e. usually the
-- beginning of the .xz file).
indexIterStreamUncompressedOffset :: IndexIter -> GettableStateVar VLI
indexIterStreamUncompressedOffset iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter  {# get lzma_index_iter.stream.uncompressed_offset #}

-- | Compressed size of this Stream.
--
-- This includes all headers except the possible Stream Padding after this
-- Stream.
indexIterStreamCompressedSize :: IndexIter -> GettableStateVar VLI
indexIterStreamCompressedSize iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.stream.compressed_size #}

-- | Uncompressed size of this Stream.
indexIterStreamUncompressedSize :: IndexIter -> GettableStateVar VLI
indexIterStreamUncompressedSize iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.stream.uncompressed_size #}

-- | Size of Stream Padding after this Stream.
indexIterStreamPadding :: IndexIter -> GettableStateVar VLI
indexIterStreamPadding iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.stream.padding #}

-- | Block number in the file.
--
-- The first Block is 1.
indexIterBlockNumberInFile :: IndexIter -> GettableStateVar VLI
indexIterBlockNumberInFile iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.block.number_in_file #}

-- | Compressed start offset of this Block.
--
-- This offset is relative to the beginning of the 'Index' (i.e. usually the
-- beginning of the .xz file). Normally this is where you should seek in the
-- .xz file to start decompressing this Block.
indexIterBlockCompressedFileOffset :: IndexIter -> GettableStateVar VLI
indexIterBlockCompressedFileOffset iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.block.compressed_file_offset #}

-- | Uncompressed start offset of this Block.
--
-- This offset is relative to the beginning of the 'Index' (i.e. usually the
-- beginning of the .xz file).
--
-- When doing random-access reading, it is possible that the target offset is
-- not exactly at Block boundary. One will need to compare the target offset
-- against 'indexIterBlockUncompressedFileOffset' or
-- 'indexIterBlockUncompressedStreamOffset', and possibly decode and throw
-- away some amount of data before reaching the target offset.
indexIterBlockUncompressedFileOffset :: IndexIter -> GettableStateVar VLI
indexIterBlockUncompressedFileOffset iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter
        {# get lzma_index_iter.block.uncompressed_file_offset #}

-- | Block number in this Stream.
--
-- The first Block is 1.
indexIterBlockNumberInStream :: IndexIter -> GettableStateVar VLI
indexIterBlockNumberInStream iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.block.number_in_stream #}

-- | Compressed start offset of this Block.
--
-- This offset is relative to the beginning of the Stream containing this
-- Block.
indexIterBlockCompressedStreamOffset :: IndexIter -> GettableStateVar VLI
indexIterBlockCompressedStreamOffset iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter
        {# get lzma_index_iter.block.compressed_stream_offset #}

-- | Uncompressed start offset of this Block.
--
-- This offset is relative to the beginning of the Stream containing this
-- Block.
indexIterBlockUncompressedStreamOffset :: IndexIter -> GettableStateVar VLI
indexIterBlockUncompressedStreamOffset iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter
        {# get lzma_index_iter.block.uncompressed_stream_offset #}

-- | Uncompressed size of this Block.
--
-- You should pass this to the Block decoder if you will decode this Block.
-- It will allow the Block decoder to validate the uncompressed size.
indexIterBlockUncompressedSize :: IndexIter -> GettableStateVar VLI
indexIterBlockUncompressedSize iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.block.uncompressed_size #}

-- | Unpadded size of this Block.
--
-- You should pass this to the Block decoder if you will decode this Block.
-- It will allow the Block decoder to validate the unpadded size.
indexIterBlockUnpaddedSize :: IndexIter -> GettableStateVar VLI
indexIterBlockUnpaddedSize iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.block.unpadded_size #}

-- | Total compressed size.
--
-- This includes all headers and padding in this Block. This is useful if you
-- need to know how many bytes the Block decoder will actually read.
indexIterBlockTotalSize :: IndexIter -> GettableStateVar VLI
indexIterBlockTotalSize iter = get
  where
    get = fromIntegral <$>
      withIndexIter iter {# get lzma_index_iter.block.total_size #}

-- | Operation mode for 'indexIterNext'
{# enum lzma_index_iter_mode as IndexIterMode
  { LZMA_INDEX_ITER_ANY as IndexIterAnyMode
  -- ^ Get the next Block or Stream.
  --
  -- Go to the next Block if the current Stream has at least one Block left.
  -- Otherwise go to the next Stream even if it has no Blocks. If the Stream
  -- has no Blocks ('indexIterStreamBlockCount' == 0),
  -- 'indexIterBlock' will have undefined values.
  , LZMA_INDEX_ITER_STREAM as IndexIterStreamMode
  -- ^ Get the next Stream.
  --
  -- Go to the next Stream even if the current Stream has unread Blocks left.
  -- If the next Stream has at least one Block, the iterator will point to the
  -- first Block. If there are no Blocks, 'indexIterBlock' will have
  -- undefined values.
  , LZMA_INDEX_ITER_BLOCK as IndexIterBlockMode
  -- ^ Get the next Block.
  --
  -- Go to the next Block if the current Stream has at least one Block left. If
  -- the current Stream has no Blocks left, the next Stream with at least one
  -- Block is located and the iterator will be made to point to the first Block
  -- of that Stream.
  , LZMA_INDEX_ITER_NONEMPTY_BLOCK as IndexIterNonEmptyBlockMode
  -- ^ Get the next non-empty Block.
  --
  -- This is like 'IndexIterBlockMode' except that it will skip Blocks whose
  -- Uncompressed Size is zero.
  }
  deriving (Show, Eq) #}

------------------------------------------------------


-- | Calculate memory usage of 'Index'.
--
-- On disk, the size of the Index field depends on both the number of Records
-- stored and how big values the Records store (due to variable-length integer
-- encoding). When the Index is kept in 'Index' structure, the memory usage
-- depends only on the number of Records/Blocks stored in the Index(es), and in
-- case of concatenated 'Index'es, the number of Streams. The size in RAM is
-- almost always significantly bigger than in the encoded form on disk.
--
-- This function calculates an approximate amount of memory needed hold the
-- given number of Streams and Blocks in 'Index' structure. This value may
-- vary between CPU architectures and also between liblzma versions if the
-- internal implementation is modified.
{# fun index_memusage as ^
  { fromIntegral `VLI' -- ^ Number of streams
  , fromIntegral `VLI' -- ^ Number of blocks
  } -> `Word64'
  #}

-- | Calculate the memory usage of an existing 'Index'.
--
-- This is a shorthand for
--
-- @
--   do
--     sc <- 'indexStreamCount' i
--     bc <- 'indexBlockCount' i
--     'indexMemusage' sc bc
-- @
{# fun index_memused as ^
  { `Index' } -> `Word32'
  #}

-- | Allocate and initialize a new 'Index' structure.
--
-- On success, a pointer to an empty initialized 'Index' is returned.
-- If allocation fails, NULL is returned.
{# fun index_init as ^ { passNullPtr- `Ptr ()' } -> `Index' #}

-- | Deallocate 'Index'.
--
-- Typically this function is not necessary because all the functions which
-- allocate 'Index' in this library uses 'ForeignPtr' and therefore run the
-- finalizer when the 'Index' is garbage collected.
{# fun index_end as ^
  { `Index'
  , passNullPtr- `Ptr ()'
  } -> `()'
  #}

-- | Add a new Block to 'Index'.
--
-- Appending a new Block does not invalidate iterators. For example, if an
-- iterator was pointing to the end of the 'Index', after 'indexAppend'
-- it is possible to read the next Block with an existing iterator.
{# fun index_append as ^
  { `Index'
  -- ^ Pointer to a 'Index' structure
  , passNullPtr- `Ptr ()'
  -- ^ Pointer to lzma_allocator, or NULL to use malloc()
  , fromIntegral `VLI'
  -- ^ Unpadded Size of a Block. This can be calculated with
  -- 'blockUnpaddedSize' after encoding or decoding the Block.
  , fromIntegral `VLI'
  -- ^ Uncompressed Size of a Block. This can be taken directly from
  -- 'Block' structure after encoding or decoding the Block.
  } -> `Ret' toRet
  #}

-- | Set the Stream Flags.
--
-- Set the Stream Flags of the last (and typically the only) Stream in 'Index'.
-- This can be useful when reading information from the 'Index', because to
-- decode Blocks, knowing the integrity check type is needed.
--
-- The given Stream Flags are copied into internal preallocated structure in
-- the 'Index', thus the caller doesn't need to keep the 'StreamFlags'
-- available after calling this function.
--
-- Returns:
--
--  * 'Ok'
--  * 'OptionsError': Unsupported 'streamFlagsVersion'.
--  * 'ProgError'

{# fun index_stream_flags as ^
  { `Index'
  , `StreamFlags'
  } -> `Ret' toRet
  #}

-- | Get the types of integrity Checks.
--
-- If 'indexStreamFlags' is used to set the Stream Flags for every
-- Stream, 'indexChecks' can be used to get a bitmask to indicate which
-- Check types have been used. It can be useful e.g. if showing the Check types
-- to the user.
--
-- The bitmask is 1 << check_id, e.g. CRC32 is 1 << 1 and SHA-256 is 1 << 10.
{# fun index_checks as ^
  { `Index' } -> `[Check]' parseChecks
  #}
  where
    parseChecks :: CUInt -> [Check]
    parseChecks (fromIntegral -> (bitMask :: Word32)) =
      unfoldr go (Just minBound)
      where
        go check'm = do
          check <- check'm
          guard $ check <= maxBound
          let check' = do
                guard $ check /= maxBound
                return $! succ check
          if bitMask .&. shiftL 1 (fromEnum check) > 0
            then return (check, check')
            else go check'

-- | Set the amount of Stream Padding.
--
-- Set the amount of Stream Padding of the last (and typically the only) Stream
-- in the 'Index'. This is needed when planning to do random-access reading
-- within multiple concatenated Streams.
--
-- By default, the amount of Stream Padding is assumed to be zero bytes.
{# fun index_stream_padding as ^
  { `Index'
  , fromIntegral `VLI'
  } -> `Ret' toRet
  #}

-- | Get the number of Streams.
{# fun index_stream_count as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Get the number of Blocks.
--
-- This returns the total number of Blocks in 'Index'. To get number of
-- Blocks in individual Streams, use 'IndexIter'.
{# fun index_block_count as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Get the size of the 'Index' field as bytes.
--
-- This is needed to verify the Backward Size field in the Stream Footer.
{# fun index_size as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Get the total size of the Stream.
--
-- If multiple 'Index'es have been combined, this works as if the Blocks
-- were in a single Stream. This is useful if you are going to combine Blocks
-- from multiple Streams into a single new Stream.
{# fun index_stream_size as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Get the total size of the Blocks.
--
-- This doesn't include the Stream Header, Stream Footer, Stream Padding, or
-- Index fields.
{# fun index_total_size as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Get the total size of the file.
--
-- When no 'Index'es have been combined with 'indexCat' and there is
-- no Stream Padding, this function is identical to 'indexStreamSize'.
-- If multiple 'Index'es have been combined, this includes also the headers
-- of each separate Stream and the possible Stream Padding fields.
{# fun index_file_size as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Get the uncompressed size of the file.
{# fun index_uncompressed_size as ^
  { `Index' } -> `VLI' fromIntegral
  #}

-- | Initialize an iterator.
--
-- This function associates the iterator with the given 'Index', and calls
-- 'indexIterRewind' on the iterator.
--
-- This function doesn't allocate any memory, thus there is no
-- @indexIterEnd@. The iterator is valid as long as the associated
-- 'Index' is valid, that is, until 'indexEnd' or using it as source
-- in 'indexCat'. Specifically, 'Index' doesn't become invalid if new
-- Blocks are added to it with 'indexAppend' or if it is used as the
-- destination in 'indexCat'.
--
-- It is safe to make copies of an initialized 'IndexIter', for example, to
-- easily restart reading at some particular position.
indexIterInit
  :: Index
  -- ^ 'Index' to which the iterator will be associated
  -> IO IndexIter
indexIterInit index = do
  iterFPtr <- mallocForeignPtrBytes {# sizeof lzma_index_iter #}
  withForeignPtr iterFPtr $ \iterPtr ->
    {# call lzma_index_iter_init #} iterPtr index
  return $ IndexIter iterFPtr

-- | Rewind the iterator.
--
-- Rewind the iterator so that next call to 'indexIterNext' will return
-- the first Block or Stream.
{# fun index_iter_rewind as ^ { `IndexIter' } -> `()' #}

-- | Get the next Block or Stream.
--
-- If next Block or Stream matching the mode was found, 'IndexIter' is updated
-- and this function returns /'False'/. If no Block or Stream matching the mode
-- is found, 'IndexIter' is not modified and this function returns 'True'. If
-- mode is set to an unknown value, 'IndexIter' is not modified and this
-- function returns 'True'.
{# fun index_iter_next as ^
  { `IndexIter'
  -- ^ Iterator initialized with 'indexIterInit'.
  , `IndexIterMode'
  -- ^ Specify what kind of information the caller wants to get.
  } -> `Bool'
  #}

-- | Locate a Block.
--
-- If it is possible to seek in the .xz file, it is possible to parse the
-- 'Index' field(s) and use 'indexIterLocate' to do random-access
-- reading with granularity of Block size.
{# fun index_iter_locate as ^
  { `IndexIter'
  -- ^ Iterator that was earlier initialized with 'indexIterInit'.
  , fromIntegral `VLI'
  -- ^ Uncompressed target offset which the caller would like to locate from
  -- the 'Stream'.
  } -> `Bool'
  #}

-- | Concatenate 'Index'es.
--
-- Concatenating 'Index'es is useful when doing random-access reading in
-- multi-'Stream' .xz file, or when combining multiple 'Stream's into single
-- 'Stream'.
{# fun index_cat as ^
  { `Index'
  -- ^ Destination index after which the source index is appended.
  , `Index'
  -- ^ Source index to be appended after the destination index.
  --
  -- If this function succeeds, the memory allocated for src is freed or moved
  -- to be part of dest, and all iterators pointing to src will become invalid.
  , passNullPtr- `Ptr ()'
  -- ^ Custom memory allocator
  } -> `Ret' toRet
  #}

-- | Duplicate 'Index'.
--
-- Returns a copy of the 'Index', or NULL if memory allocation failed.
{# fun index_dup as ^
  { `Index'
  , passNullPtr- `Ptr ()'
  } -> `Index'
  #}

-- | Initialize .xz index encoder.
--
-- The valid 'Action' values for 'code' are 'Run' and 'Finish'. It
-- is enough to use only one of them (you can choose freely; use 'Run' to
-- support liblzma versions older than 5.0.0).
{# fun index_encoder as ^
  { `Stream'
  , `Index'
  } -> `Ret' toRet
  #}

-- | Initialize .xz Index decoder.
--
-- The valid 'Action' values for 'code' are 'Run' and 'Finish'. It
-- is enough to use only one of them (you can choose freely; use 'Run' to
-- support liblzma versions older than 5.0.0).
indexDecoder
  :: Stream
  -- ^ Properly prepared 'Stream'
  -> Word64
  -- ^ How much memory the resulting 'Index' is allowed to require.
  -> IO (Ret, ForeignPtr Index)
indexDecoder stream (fromIntegral -> memLimit) = do
  -- double pointer to an index
  (indexFPtr :: ForeignPtr Index) <- mallocForeignPtr
  withForeignPtr indexFPtr $ \(indexPtr :: Ptr Index) -> do
    (toRet -> ret) <-
      {# call lzma_index_decoder #} stream indexPtr memLimit
    return (ret, indexFPtr)

passNullPtr :: (Ptr a -> b) -> b
passNullPtr f = f nullPtr

#if DEBUG
instance PrettyVal StreamFlags where
  prettyVal = Pretty.String . show
#endif
