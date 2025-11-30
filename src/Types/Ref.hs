{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |

module Types.Ref where

import Flat
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString qualified as BS

import Data.Word

import Types.Extent qualified as Extent
import Types.Dev (Dev)

import Instances
import qualified Data.ByteString as BS
import Control.Monad (guard)

type Length = Word64
type Offset = Word32

data Ref a = RefInline BS.ByteString
           | RefRaw Dev Offset Length     -- data stored directly
           | ExtentRef Extent.Id  -- data stored indirectly via the extent system
           deriving (Eq, Show)
