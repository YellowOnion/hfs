-- |

module Backend.Super where


import Control.Concurrent.STM
import Control.Concurrent ( forkIO )
import System.IO
import Data.ByteString.Lazy as LBS

import Control.Monad ( forever, when )
import Data.Binary


import Types.Super
import Util ( withTMVar )
import Types.Bucket ( bUCKET_SIZE )
import Instances

import Debug.Trace

superWriter :: TMVar Handle -> SuperManager -> IO ()
superWriter _h SuperManager{..} = loop
  where
    loop = do
        (ctl, sb) <- atomically $ (,) <$> takeTMVar _ctl <*> readTMVar _super
        when (ctl == SuperWrite) $ withTMVar _h $ \handle -> do
                hSeek handle AbsoluteSeek 0
                LBS.hPut handle . traceShowId . encode $ sb
                hFlush handle
                loop


openSuper :: TMVar Handle -> IO SuperManager
openSuper _handle = do
  bytes <- withTMVar _handle $ \handle -> do
    hSeek handle AbsoluteSeek 0
    LBS.hGet handle (fromIntegral bUCKET_SIZE)
  let sb = case decodeOrFail bytes of
               Left (_,_,e) -> error (show e)
               Right (_, _,sb) -> sb

  when (magic sb /= mAGIC) $ error "not a hfs file"

  sbm <- atomically $ SuperManager <$> newTMVar sb <*> newEmptyTMVar
  _ <- forkIO $ superWriter _handle sbm

  traceIO $ "trace: " ++ (show sb)

  return sbm

closeSuper :: SuperManager -> IO ()
closeSuper SuperManager{..} = atomically $ putTMVar _ctl SuperExit
