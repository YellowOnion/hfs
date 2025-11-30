-- |

module Types.Store where

import System.IO (Handle)
import Data.Map.Strict qualified as Map
import Control.Concurrent.STM

import Data.Word

import Types.Super qualified as Super
import Types.Bucket qualified as Bucket
import Types.Extent qualified as Extent

data Store = Store
  { _handle :: TMVar Handle
  , superManager :: Super.SuperManager
  , _inodeCount :: TMVar Word64
  , btrees :: BTree.Manager
  }
