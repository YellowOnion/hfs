-- |

module Backend.Bucket where

import System.IO
import Data.ByteString.Lazy as LBS

import Data.Binary

import Data.Map.Strict qualified as Map
import Control.Concurrent.STM

import Types.Bucket hiding (Id(..))
import Types.Bucket qualified as Bucket
import Types.Super qualified as Super
import Types.Store
import Types.Ref
import Types.BTree qualified as BTree

import Backend.Super (openSuper)
import Types.Super (Super(allocInfo))
import qualified Types.Bucket as Bucket
import qualified Types.Bucket as Bucket

bucketIdToRef (Bucket.Id dev offset) = RefRaw dev (Bucket.bUCKET_SIZE * offset) Bucket.bUCKET_SIZE

seekToRef handle (RawRef _ offset _) = hSeek handle AbsoluteSeek offset

_openFile :: FilePath -> IO Handle
_openFile p = do
  h <- openBinaryFile p ReadWriteMode
  hSetBuffering h (BlockBuffering (Just $ fromIntegral bUCKET_SIZE))
  return h

formatStore :: FilePath -> IO Handle
formatStore p = do
  handle <- openBinaryFile p ReadWriteMode
  LBS.hPut handle . encode $ Super.Super
    { Super.magic = Super.mAGIC
    , Super.version = 1
    , Super.journalSeq = 0
    , Super.superSeq = 0
    , mapInfo = [ (bucketIdToRef 0 1)
                , (bucketIdToRef 0 2)]
    }
  seekToRef (bucketIdToRef 0 1)
  LBS.hPut handle . encode $ defaultAlloc
  seekToRef (bucketIdToRef 0 2)
  LBS.hPut handle . encode $ Map.empty
  hFlush handle
  return handle


openStore :: FilePath -> IO Store
openStore p = _openFile p >>= openStore2

openStore2 :: Handle -> IO Store
openStore2 _handle = do
  handle <- newTMVarIO _handle
  sbm <- openSuper handle
  (_alloc, _inodeCount, _extents) <- atomically $ do
    sb@Super.Super{..} <- takeTMVar $ Super._super sbm
    a <- newTMVar allocInfo
    i <- newTMVar inodeCount
    e <- newTMVar extentInfo
    putTMVar (Super._super sbm) sb
    return (a, i, e)
  return $ Store handle sbm _alloc _inodeCount _extents


defaultAlloc = Map.insert (Bucket.Id 0 0) (Alloc AllocSuper 0)
             . Map.insert (Bucket.Id 0 1) (Alloc (AllocBtree 0) 0) -- Alloc Btree
             . Map.insert (Bucket.Id 0 3) (Alloc (AllocBtree 0) 0) -- Extent Btree
             $ Map.empty

-- this allocates a new bucket for writes
{-
allocBucket :: Store k v -> IO (TMVar Bucket)
allocBucket (Store _h _s _obs) = do
  return (_)
printStore :: Store -> IO ()
printStore Store{..} = do
  (sbm, ) <- atomically $ (,) <$> (readTMVar . Super._super $ superManager) <*> readTMVar
  print sb
  print ob
-}
