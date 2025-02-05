-- |

module Backend.Bucket where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Flat hiding (size)

import Control.Concurrent.STM

import Types.Bucket
import Types.Super qualified as Super


data Store k v = Store
  { storeHandle :: TMVar Handle
  , storeSuper :: TMVar Super.Super
  , storeOpenBuckets :: TMVar [BucketIdx]
  }

formatStore :: FilePath -> IO (Handle)
formatStore path = do
  handle <- openBinaryFile path ReadWriteMode
  LBS.hPut handle . flat $ Super.Super
    { Super.magic = Super.mAGIC
    , Super.version = 1
    , Super.journalLoc = BucketIdx 0 1
    , Super.journalSeq = 0
    , Super.superSeq = 0 }
  hFlush handle
  return handle

openStore :: (Flat k, Flat v, Ord k) => FilePath -> IO (Maybe (Store k v))
openStore path = do
  handle <- openBinaryFile path ReadWriteMode
  mhandle <- newTMVarIO handle
  bytes <- BS.hGet handle bUCKET_SIZE
  msuper <- newTMVarIO $ unflat bytes


openBucket :: Store -> BucketIdx -> IO (TMVar Bucket)
openBucket (Store h s)idx@BucketIdx{..} writer = do
  return $ bracket (attomically . takeTMVar h)
                   (attomically . putTMVar h)
                 $ \handle -> do
    hSeek handle AbsoluteSeek (offset * bUCKET_SIZE)
    bytes <- BS.hGet handle bUCKET_SIZE
    mclose <- newEmptyTMVarIO
    let bucket = case unflat bytes of
          Left e -> error (show e)
          Right (header, _data) -> Bucket
            { dataType = headerType header
            , position = idx
            , gen = headerGen header
            , journalSeq = 0
            , size = headreSize header
            , bdata = _data
            , close = mclose }
        loop = do
      t <- attomically $ isEmptyTMVar mclose
      unless t $ loop
    forkIO loop
