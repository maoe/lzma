{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Codec.Compression.LZMA.Internal.Types
  ( ReadRequest(..)
  , Position
  , Compression(..)
  , Size

  -- * Exceptions
  , SomeLZMAException(..)
  , lzmaExceptionToException
  , lzmaExceptionFromException
  , LZMAException(..)
  , C.ErrorCode(..)
  ) where
import Control.Exception
import Data.Typeable (Typeable, cast)

import Data.Tagged (Tagged)

import qualified Codec.Compression.LZMA.Internal.C as C

-- | Read request to upstream.
--
-- [@'PReadWithSize' pos size@] Seek to @pos@ and read @size@ bytes.
--   This constructor is used only for asking for compressed bytes in the index
--   decoder.
-- [@'PRead' pos@] Seek to @pos@ and read bytes.
-- [@'Read'@] Read next bytes from the current position.
data ReadRequest (c :: Compression) where
  PReadWithSize :: Position 'Compressed -> Size -> ReadRequest 'Compressed
  --- ^ Seek to the position and read the given number of bytes. This constructor
  -- is used only for asking for compressed bytes.
  PRead :: Position c -> ReadRequest c
  --- ^ Seek to the position and read bytes.
  Read :: ReadRequest c
  --- ^ Read next bytes from the current position.

deriving instance Show (ReadRequest c)

-- | Absolute position in a file.
type Position (c :: Compression) = Tagged c Integer

data Compression = Compressed | Uncompressed
  deriving (Eq, Show)

type Size = Int

data SomeLZMAException = forall e. Exception e => SomeLZMAException e
  deriving Typeable

instance Show SomeLZMAException where
  show (SomeLZMAException e) = show e

instance Exception SomeLZMAException

lzmaExceptionToException :: Exception e => e -> SomeException
lzmaExceptionToException = toException . SomeLZMAException

lzmaExceptionFromException :: Exception e => SomeException -> Maybe e
lzmaExceptionFromException x = do
  SomeLZMAException e <- fromException x
  cast e

-- | Error code thrown by liblzma
data LZMAException = LZMAErrorCode C.ErrorCode
  deriving (Typeable, Show)

instance Exception LZMAException where
  toException = lzmaExceptionToException
  fromException = lzmaExceptionFromException
