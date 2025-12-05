{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Super where

import Flat
import Data.Binary qualified as Binary

import Data.Word
import Data.Char (ord)
import Data.Bits
import Control.Concurrent.STM
import Data.Map.Strict qualified as Map

import Types.Bucket qualified as Bucket
import Types.Extent qualified as Extent
import Types.BTree qualified as BTree
import Types.Ref

data MAGIC = MAGIC
  !Word64
  !Word64
  deriving (Eq, Show, Generic, Flat, Binary.Binary)

mAGIC :: MAGIC
mAGIC = MAGIC (stringToWord64 "hsFSFTWW") (stringToWord64 "00000001")

stringToWord64 :: String -> Word64
stringToWord64 = fromIntegral . sum . zipWith (\o c -> ord c `shiftL` o) [0,8..] . reverse

data Super = Super
  { magic      :: !MAGIC
  , version    :: !Word64
  , journalSeq :: !Word64
  , journalBuckets :: ![Bucket.Id]
  , superSeq   :: !Word64
  , inodeCount :: !Word64
  --, mapInfo :: ![Ref BTree.Map]
  } deriving (Eq, Show, Generic, Binary.Binary)

data SuperCtl = SuperWrite | SuperExit deriving (Eq, Show)

data SuperManager = SuperManager
  { _super :: TMVar Super
  , _ctl :: TMVar SuperCtl
  }
