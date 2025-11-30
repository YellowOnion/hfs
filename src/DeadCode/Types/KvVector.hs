{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- |

module Types.KvVector where

import Control.Monad.STM
import Control.Concurrent.STM

import Data.Vector qualified as Vec


import Types.VNode
import Data.List (sortOn)

data KvVector k v = KvVector
  { kvVecMax :: Int
  , kvVecNodeList  :: TVar (Vec.Vector (VItem k v))
  }

type instance KeyType (KvVector k v) = k
type instance ValueType (KvVector k v) = v

instance RunVNode IO (KvVector) where
  runVNode = runKvVector

runKvVector :: (KeyType t ~ k) => VNode t (KvVector k v) k a -> IO a
runKvVector a = case a of
  Create -> KvVector 32
               <$> newTVarIO Vec.empty

  (Delete _) -> return ()

  (Size (KvVector s _)) -> return s

  (Length KvVector{..}) -> atomically $ do
    vec <- readTVar kvVecNodeList
    return $ Vec.length vec

  (Append k v KvVector{..}) -> atomically $ do
    modifyTVar kvVecNodeList $ flip Vec.snoc (VItem k v)
    vec <- readTVar kvVecNodeList
    return $ (Vec.length vec) >= kvVecMax

  (Remove k KvVector{..}) -> atomically $ do
    modifyTVar kvVecNodeList $ flip Vec.snoc (VItemDel k)
    return ()


  (Fold f init KvVector{..}) -> atomically $ do
    vec <- readTVar kvVecNodeList
    return $ Vec.foldl' f init vec
{-

  (Split KvVector{..}) -> atomically $ let

    new' :: Internal i
                  -> External i -> STM (KvVector i)
    new' n l = KvVector kvVecMax <$> newTVar (Vec.length n)
                                 <*> newTVar (Vec.length l)
                                 <*> newTVar n
                                 <*> newTVar l

    split' = Vec.splitAt (kvVecMax `div` 4)
                 . Vec.fromList
                 . sortOn fst
                 . Vec.toList

    split'' = Vec.splitAt (kvVecMax `div` 4)
                 . Vec.fromList
                 . sortOn fst
                 . Vec.toList
    in do
    vecNodes <- readTVar kvVecNodeList
    vecLeafs :: (Ord (KeyType t) => External t) <- readTVar kvVecLeafList
    let (ln :: Internal t, un :: Internal t) = split' vecNodes
        (lf :: External t, uf :: External t) = split'' vecLeafs
    lower <- new' ln lf
    upper <- new' un uf
    return (lower, upper)

  (FlushLeafs KvVector{..}) -> atomically $ do
    writeTVar kvVecLeafCount 0
    vec <- swapTVar kvVecLeafList Vec.empty
    return . sortOn fst $ Vec.toList vec

  (Size (KvVector s _ _ _ _)) -> return s

  (FindNode k KvVector{..}) -> atomically $ do
    vn <- readTVar kvVecNodeList
    return $ Vec.find ((< k) . fst) vn

  (FindLeaf k KvVector{..}) -> atomically $ do
      vl <- readTVar kvVecLeafList
      let ls = Vec.foldr' (\a -> if fst a == k then (a:) else id) [] vl
      return $ case ls of
        [] -> Nothing
        (a:_) -> Just a
-}
