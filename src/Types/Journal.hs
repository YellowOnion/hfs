{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Journal where

import Flat


import Types.Ref
import Types.BTree qualified as BTree

import Data.Word

data Entry a k v = NextBucket
  | Checkpoint
  { sequence :: !Word64
  , rootNode :: !(Ref BTree.BRoot) }
  | Insert
  { sequence :: !Word64
  , btreeType :: !BTree.Type
  , key      :: !k
  , value    :: !v }
  | Delete
  { sequence :: !Word64
  , btreeType :: !BTree.Type
  , key :: !k }
  deriving (Eq, Show, Generic, Flat)
