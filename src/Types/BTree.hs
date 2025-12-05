{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
-- |

module Types.BTree where

import Data.Binary
import GHC.Generics

import Flat

import Data.Map.Strict qualified as Map

import Types.Ref

import Types.Extent qualified as Extent
import Types.Bucket qualified as Bucket

import Control.Concurrent.STM

type family Key name
type family Value name
type family BTreeType name

data Type = Alloc
          | Extent
           deriving (Eq, Ord, Enum, Show, Generic, Flat )

data BRoot = BRoot
  { alloc :: BNode Bucket.Id Bucket.Alloc
  , extent :: BNode Extent.Id Extent.Ptr
  } deriving (Eq, Show, Generic, Binary)

data BNode k v = BLeaf k v
               | BNodes [BNode k v]
               deriving (Eq, Show, Generic, Flat, Binary)

data Ctl = Write | Exit deriving (Eq, Show)

data Op = OpAllocInsert  !Bucket.Id !Bucket.Alloc
        | OpExtentInsert !Extent.Id !Extent.Ptr
        | OpAllocDelete  !Bucket.Id
        | OpExtentDelete !Extent.Id
        deriving (Eq, Show)

data Manager = Manager
  { _btree      :: !(TMVar BRoot)
  , _txs        :: !(TMVar [ Op ])
  , _ctl        :: !(TMVar Ctl)
  , _journalManager :: !(TMVar JournalManager)
  }

data JournalManager = JournalManager
  { seq             :: !Word64
  , bucketCurrent   :: !Bucket.Id
  , bucketBytesLeft :: !Word64
  , buckets         :: ![Bucket.Id]
  } deriving (Eq, Show)


--data Map_ = Alloc_ (Map.Map Bucket.Id (Ref Bucket.Alloc))
--         | Extent_ (Map.Map Extent.Id (Ref Extent.Ptr))
--         deriving (Eq, Show, Generic, Binary)


--data Map = Map
--  { allocMap :: Map.Map Bucket.Id (Ref Bucket.Alloc)
--  , extentMap :: Map.Map Extent.Id (Ref Extent.Ptr)
--  } deriving (Eq, Show)

--insert :: k -> v -> MapType -> Map -> Map
