-- |

module Backend.VectorSTM where



import Control.Monad.STM
import Control.Concurrent.STM

import Data.Vector qualified as Vec
import Control.Monad qualified as Ctrl

import Data.List (sortOn)
import Data.Int

import Types.VItem

import Prelude hiding (foldl)

data VectorSTM k v = VectorSTM
  { vecMax :: Int
  , vecNodeList  :: TVar (Vec.Vector (TVar (VItem k v)))
  }

create :: STM (VectorSTM k v)
create = VectorSTM 32 <$> newTVar Vec.empty

delete :: VectorSTM k v -> STM ()
delete _ = return ()

append :: k -> v -> VectorSTM k v -> STM (Bool)
append k v VectorSTM{..} = do
    modifyTVar vecNodeList $ flip Vec.snoc (VItem k v)
    vec <- readTVar vecNodeList
    return $ (Vec.length vec) >= vecMax

remove :: k -> VectorSTM k v -> STM ()
remove k VectorSTM{..} = do
  modifyTVar vecNodeList $ flip Vec.snoc (VItemDel k)
  return ()

length :: VectorSTM k v -> STM Int
length VectorSTM{..} = do
  vec <- readTVar vecNodeList
  return $ Vec.length vec

foldl f init VectorSTM{..} = do
  vec <- readTVar vecNodeList
  return $ Vec.foldl' f init vec

for v init f = foldl f init v

foldM f i VectorSTM{..} = do
  vec <- readTVar vecNodeList
  Ctrl.foldM f i vec
