{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- |

module Backend.BTree where

import Control.Concurrent.STM
import System.IO
import Data.ByteString.Lazy qualified as LBS

import Control.Monad (when)

import Types.Journal qualified as J
import Types.Extent  qualified as Extent
import Types.Bucket  qualified as Bucket
import Types.Super   qualified as Super

import Types.BTree   qualified as BTree

import Flat qualified as Flat
import Util (withTMVar)
import Types.Bucket (bUCKET_SIZE)

import Prelude hiding (seq)

btreeWriter :: TMVar Handle -> BTree.Manager -> IO ()
btreeWriter _handle BTree.Manager{..} = loop
  where
    loop :: IO ()
    loop = do
          (ctl, txs) <- atomically $
                         (,) <$> takeTMVar _ctl
                         <*> takeTMVar _txs
          when (ctl == BTree.Write) $ do
            mapM_ (journalWrite _journalManager _handle) txs
            loop

journalWrite :: TMVar BTree.JournalManager
             -> TMVar Handle
             -> BTree.Op
             -> IO BTree.JournalManager
journalWrite _jm _handle tx = (atomically $
                               (,) <$> takeTMVar _jm
                                   <*> takeTMVar _handle) >>=
  \(jm@BTree.JournalManager{..}, handle) -> do
   let dat = LBS.fromStrict $ encodeEntry tx seq
       len = LBS.length dat

   if bucketBytesLeft >= (fromIntegral len)
     then do
        seekToBucketWithOffset handle bucketCurrent (fromIntegral bucketBytesLeft)
        LBS.hPut handle dat
        return $ jm { BTree.bucketBytesLeft = bucketBytesLeft - (fromIntegral len) }
     else return $ error "TODO implement bucket rotation in journal"
  where
    seekToBucketWithOffset h (Bucket.Id _ bOffset) bl =
      hSeek h AbsoluteSeek
                     $ (fromIntegral Bucket.bUCKET_SIZE)
                     * (fromIntegral bOffset + 1) - bl
    encodeEntry (BTree.OpAllocInsert k v) jseq =
      Flat.flat $ J.Insert jseq BTree.Alloc k v
    encodeEntry (BTree.OpExtentInsert k v)  jseq =
      Flat.flat $ J.Insert jseq BTree.Extent k v
    encodeEntry _ _ = error "Non-implemented Journal entry"


-- TODO this shouldn't be named open, it's code for a fresh FS
open :: Super.Super -> IO (BTree.Manager)
open Super.Super{..} = do
  let (firstBucket : tailBuckets) = journalBuckets
      broot = BTree.BRoot (BTree.BNodes []) (BTree.BNodes [])
      jm = BTree.JournalManager
                journalSeq
                firstBucket
                (fromIntegral bUCKET_SIZE)
                tailBuckets
  _jm <- atomically $ newTMVar jm
  atomically $ BTree.Manager
            <$> newTMVar broot
            <*> newTMVar []
            <*> newEmptyTMVar
            <*> newTMVar jm
