{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Super where

import Flat

import Data.Word

import Types.Bucket as Bucket

data MAGIC = MAGIC
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  deriving (Eq, Show, Generic, Flat)

mAGIC = MAGIC 0x4861736B656C6C46 0x53

data Super = Super
  { magic :: {-# UNPACK #-} !MAGIC
  , version :: {-# UNPACK #-} !Word64
  , journalLoc :: {-# UNPACK #-} !Bucket.BucketIdx
  , journalSeq :: {-# UNPACK #-} !Word64
  , superSeq   :: {-# UNPACK #-} !Word64
  , bucketCount :: {-# UNPACK #-} !Word64
  , rootBucket :: {-# UNPACK #-} !Bucket.BucketIdx
  } deriving (Eq, Show, Generic, Flat)
