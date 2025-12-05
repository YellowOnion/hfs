-- |

module Util where

import Control.Concurrent.STM
import Control.Exception (bracket)

import Debug.Trace

withTMVar :: TMVar a -> (a -> IO c) -> IO c
withTMVar mvar = bracket
  (atomically $ takeTMVar mvar)
  (atomically . putTMVar mvar)


shouldTrace :: Bool
shouldTrace = True

dbTraceIO :: Show a => a -> IO ()
dbTraceIO s  = if shouldTrace
               then traceIO $ "trace: " ++ show s
               else return ()
