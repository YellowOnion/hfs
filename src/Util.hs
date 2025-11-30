-- |

module Util where

import Control.Concurrent.STM
import Control.Exception (bracket)


withTMVar :: TMVar a -> (a -> IO c) -> IO c
withTMVar mvar = bracket
  (atomically $ takeTMVar mvar)
  (atomically . putTMVar mvar)
