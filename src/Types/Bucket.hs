{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- |

module Types.Bucket where

import Flat
import Data.Binary
import Instances
import Data.Int
import Data.Word
import Data.ByteString qualified as BS

import Control.Concurrent.STM

import Types.Dev

bUCKET_SIZE :: Int64
bUCKET_SIZE = 2 ^ (18 :: Int) -- 256kB hard coded for now

data BucketType = Free
                | Super
                | Journal
                | Btree
                | User deriving (Eq, Show, Generic, Flat)

data Id = Id
  { dev :: !Dev
  , offset :: !Word32 -- amount of bUCKET_SIZE from start of dev
  } deriving (Eq, Ord, Show, Generic, Binary)

data AllocInfo = AllocInfo
  { _dirtyBytes :: !Word32 -- do I need anything else???
  } deriving (Eq, Show, Generic, Flat)

data AllocType where
  AllocFree    ::              AllocType
  AllocSuper   ::              AllocType
  AllocJournal :: AllocInfo -> AllocType
  AllocBtree   :: AllocInfo -> AllocType
  AllocUser    :: AllocInfo -> AllocType
  deriving (Eq, Show, Generic, Flat)

data Alloc = Alloc
 {  _allocType :: !AllocType
  , _journalSeq :: !Word64
  } deriving (Eq, Show, Generic, Flat)

dirtyBytes :: Alloc -> Word32
dirtyBytes = go . _allocType
  where
    go AllocFree        = 0
    go AllocSuper       = fromIntegral bUCKET_SIZE
    go (AllocJournal a) = _dirtyBytes a
    go (AllocBtree   a) = _dirtyBytes a
    go (AllocUser    a) = _dirtyBytes a
