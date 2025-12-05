{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Extent where

import Data.Binary
import Data.Word
import GHC.Generics

import Types.Dev

import Flat

data Csum = Crc32 | CSumNil deriving (Eq, Show, Generic, Flat, Binary)
data Comp = CompNil deriving (Eq, Show, Generic, Flat, Binary)

data Ptr = Ptr
  { dev :: Dev
  , devOffset :: Word32 -- bcachefs uses 44bits???
  , csum :: Maybe Csum
  , comp :: Maybe Comp
  , next :: Ptr
  } | Nil
  deriving (Eq, Show, Generic, Flat, Binary)

data Id = Id
  { inode :: Word64
  , inodeOffset :: Word64
  --, snapshot :: Word32
  } deriving (Eq, Ord, Show, Generic, Flat, Binary)
