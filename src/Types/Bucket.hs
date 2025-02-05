{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Bucket where

import Flat

import Data.Int
import Data.Word
import Data.ByteString qualified as BS

import Control.Concurrent.STM

bUCKET_SIZE :: Int64
bUCKET_SIZE = 2^18 -- 256kB hard coded for now

data BucketType = Free
                | Super
                | Journal
                | Btree
                | User deriving (Eq, Show, Generic, Flat)


data BucketIdx = BucketIdx
  { device :: {-# UNPACK #-} !Word16
  , offset :: {-# UNPACK #-} !Word64
  } deriving (Eq, Show, Generic, Flat)

data Bucket = Bucket
  { dataType   :: !BucketType
  , position   :: !BucketIdx
  , gen        :: !Word8
  , journalSeq :: !Word64
  , size       :: !Word32
  , bData      :: BucketData
  , close      :: TMVar ()
  } deriving (Eq)

data BucketHeader = BucketHeader
  { headerType :: !BucketType
  , headerSize :: !Word32 -- 4GB max far too big to be practical
  , bucketGen  :: !Word8
  , bucketCSum :: !Word32
  } deriving (Eq, Show, Generic, Flat)


newtype BucketData = BucketData
  { toByteString :: BS.ByteString } deriving (Eq, Show, Generic, Flat)
