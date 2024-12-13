{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |

module Types.KvVector where

import Control.Monad.STM
import Control.Concurrent.STM

import Data.Vector qualified as Vec


import Types.VNode
import Data.List (sortOn)

data KvVector k v = forall iv . KvVector
  { kvVecMax :: Int
  , kvVecNodeCount :: TVar Int
  , kvVecLeafCount :: TVar Int
  , kvVecNodeList  :: TVar (Vec.Vector (k, iv))
  , kvVecLeafList  :: TVar (Vec.Vector (k, v))
  }


instance Ord k => VNode KvVector IO k v
  where
    create s = KvVector s
               <$> newTVarIO 0
               <*> newTVarIO 0
               <*> newTVarIO Vec.empty
               <*> newTVarIO Vec.empty

    delete _ = return ()

    appendNode k v KvVector{..} = atomically $ do
      modifyTVar kvVecNodeList $ flip Vec.snoc (k, v)
      modifyTVar' kvVecNodeCount (+1)
      cnt <- readTVar kvVecNodeCount
      return $ cnt >= (kvVecMax `div` 2)

    appendLeaf k v KvVector{..} = atomically $ do
      modifyTVar kvVecLeafList $ flip Vec.snoc (k, v)
      modifyTVar' kvVecLeafCount (+1)
      cnt <- readTVar kvVecLeafCount
      return $ cnt >= (kvVecMax `div` 2)

    destroyNode = error "destroy nodes not implemented"
    destroyLeaf = error "destroy leafs not implemented"

    split KvVector{..} = atomically $ do
      vecNodes <- readTVar kvVecNodeList
      vecLeafs <- readTVar kvVecLeafList
      let (ln, un) = split' vecNodes
          (lf, uf) = split' vecLeafs
      lower <- new' ln lf
      upper <- new' un uf
      return (lower, upper)
      where
        new' n l = KvVector kvVecMax <$> newTVar (Vec.length n)
                                     <*> newTVar (Vec.length l)
                                     <*> newTVar n
                                     <*> newTVar l
        split' = Vec.splitAt (kvVecMax `div` 4)
                 . Vec.fromList
                 . sortOn fst
                 . Vec.toList

    flushLeafs KvVector{..} = atomically $ do
      writeTVar kvVecLeafCount 0
      vec <- swapTVar kvVecLeafList Vec.empty
      return . sortOn fst $ Vec.toList vec

    fuse        = error "fuse not implemented"

    size (KvVector s _ _ _ _) = return s

    findNode k KvVector{..} = atomically $ do
      vn <- readTVar kvVecNodeList
      return $ Vec.find ((< k) . fst) vn

    findLeaf k KvVector{..} = atomically $ do
      vl <- readTVar kvVecLeafList
      let ls = Vec.foldr' (\a -> if fst a == k then (a:) else id) [] vl
      return $ case ls of
        [] -> Nothing
        (a:_) -> Just a
