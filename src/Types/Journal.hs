{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Journal where

import Flat
import Data.Binary qualified as Binary


import Types.Ref
import Types.BTree qualified as BTree

data JournalEntry = JCheckpoint
  { sequence :: Word64
  , rootNode :: Ref BTree.Map }
  | JInsert
  { sequence :: Word64
  , key :: Btree.Key
  , value :: BTree.Value
  } deriving (Eq, Show, Generic, Binary)
