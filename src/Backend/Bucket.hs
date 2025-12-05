-- |

module Backend.Bucket where

import System.IO
import Data.ByteString.Lazy as LBS

import Data.Binary

import Control.Concurrent.STM

import Types.Bucket hiding (Id(..))
import Types.Bucket qualified as Bucket
import Types.Super qualified as Super
import Backend.BTree qualified as BTree
import Types.Store

import Backend.Super (openSuper)

{-
bucketIdToRef (Bucket.Id dev offset) = RefRaw dev (Bucket.bUCKET_SIZE * offset) Bucket.bUCKET_SIZE

seekToBucket handle (RefRaw _ offset _) = hSeek handle AbsoluteSeek offset

-}

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
    , Super.journalBuckets = defaultJournalBuckets
    , Super.superSeq = 0
    , Super.inodeCount = 0
    }
  -- TODO alloc journal buckets
  hFlush handle
  return handle


openStore :: FilePath -> IO Store
openStore p = _openFile p >>= openStore2

openStore2 :: Handle -> IO Store
openStore2 _handle = do
  handle <- newTMVarIO _handle
  sbm <- openSuper handle
  (_inodeCount, sb) <- atomically $ do
    sb@Super.Super{..} <- takeTMVar $ Super._super sbm
    i <- newTMVar inodeCount
    putTMVar (Super._super sbm) sb
    return (i, sb)
  btm <- BTree.open sb
  return $ Store handle sbm _inodeCount btm


defaultJournalBuckets :: [Bucket.Id]
defaultJournalBuckets = [ Bucket.Id 0 1
                        ]

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
