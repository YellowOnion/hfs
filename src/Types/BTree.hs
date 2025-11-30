{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
-- |

module Types.BTree where

import Data.Binary
import GHC.Generics

import Data.Map.Strict qualified as Map

import Types.Ref

import Types.Extent qualified as Extent
import Types.Bucket qualified as Bucket

type family Key name
type family Value name
type family BTreeType name

data MapType = Alloc
             | Extent
             deriving (Eq, Ord, Enum, Show)

data Map_ = Alloc_ (Map.Map Bucket.Id (Ref Bucket.Alloc))
         | Extent_ (Map.Map Extent.Id (Ref Extent.Ptr))
         deriving (Eq, Show, Generic, Binary)


data Map = Map
  { allocMap :: Map.Map Bucket.Id (Ref Bucket.Alloc)
  , extentMap :: Map.Map Extent.Id (Ref Extent.Ptr)
  } deriving (Eq, Show)

--insert :: k -> v -> MapType -> Map -> Map
