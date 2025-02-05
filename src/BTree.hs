{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module BTree where

import Debug.Trace

import Control.Monad.STM
import Control.Concurrent.STM


-- import Types.VNode qualified as VNode
import Backend.VectorSTM qualified as VecSTM
import Backend.VectorSTM (VectorSTM)
import Types.VItem
import Control.Monad (forM_, sequence, join)
import Control.Lens

import Data.List (sortOn)

-- basic btree
data Btree i = Bleaf i
             | Bnode [Btree i]

-- slightly optimized
data Btree1 i = Bleafs1 [i]
             | Bnodes1 [Btree1 i]

-- Log structured Btree with insert buffer
data Btree2 i = Bleafs2 [i]
              | Bnodes2 [Either i (Btree2 i)]

-- with polymorphic container type
data Btree3 t i = Bleafs3 (t i)
                | Bnodes3 (t (Either i (Btree3 t i)))

data BNode k v where
  BNodes :: VectorSTM k (BNode k v) -> BNode k v
  BLeafs :: VectorSTM k v -> BNode k v

data BTree m k v where
  BTree :: { length :: !Int
           , bRoot :: !(BNode k v)
           } -> BTree m k v

lookup :: (Ord k)
       => k -> BTree m k v -> Maybe (Int, v)
lookup !k BTree{..} = go 0 bRoot
  where
    go !n (BLeafs ref) = error "not implemented"
    go !n (BNodes ref) = error "not implemented"

data NeedsSplit = LeafSplit | NodeSplit

--insert :: (Monad m, VNode.VNode (BNode) m k v)
--       => k
--       -> v
--       -> BTree m t k v
--       -> m (BTree m t k v)
insert :: (Ord k, Show k) => k -> v -> BTree STM k v -> STM (BTree STM k v)
insert k v BTree{..} = do
  bt <- go (0 :: Int) bRoot
  case bt of
    Just bt -> return $ BTree (length + 1) bt
    Nothing -> error "Btree failed"
  where
 {--   go :: ( Monad m
          , VNode.VNode m t k v
          )
       => Int
       -> BNode m t k v
       -> m (BNode m t k v) -}
    split ref = do
      (n, all) <- VecSTM.for ref (0, []) $
        \(i, l) item ->
          (i+1, item : l)
      let (lower, upper) = trace ("split on: " ++ show k) $ splitAt (n `div` 2) $ sortOn (^. _k) all
      lowerNode <- VecSTM.create
      upperNode <- VecSTM.create
      forM_ lower (\(VItem k v) -> VecSTM.append k v lowerNode)
      forM_ upper (\(VItem k v) -> VecSTM.append k v upperNode)
      root    <- VecSTM.create
      _ <- VecSTM.append (lower ^. singular _last . _k) (BLeafs lowerNode) root
      _ <- VecSTM.append (upper ^. singular _head . _k) (BLeafs upperNode) root
      return . Just $ BNodes root

--    go :: Int -> BNode k v -> IO (Maybe (BNode k v))
    go !n l@(BLeafs ref)
      | n > 64 = error "insert leaf too deep"
      | otherwise = do
          needsFlush <- VecSTM.append k v ref
          case () of
            () | needsFlush && n == 0 -> split ref
               | needsFlush -> return Nothing
               | otherwise -> return . Just $ l

    go !n (BNodes ref)
      | n > 64 = error "insert node too deep"
      | otherwise = do
        m :: Maybe (VItem k v) <- VecSTM.for ref Nothing $ \m i1 -> case m of
              Just i0 -> case () of
                () | i1 ^. _k >= k -> Just i0
                   | i0 ^. _k >= i1 ^. _k -> Just i0
                   | otherwise -> Just i1
              Nothing -> if i1 ^. _k >= k
                         then Nothing
                         else Just i1
        case m of
             Nothing -> error "???"
             Just (VItem _ v) -> go (n+1) v

foldn :: (Int -> a -> VItem k v -> a) -> a -> BTree STM k v -> STM a
foldn f init BTree{..} = go 0 init bRoot
  where
    go n i (BLeafs ref) = VecSTM.foldl (f n) i ref
    go n i (BNodes ref) = VecSTM.foldM (\i (VItem _ v) -> do
                                          go (n+1) i v
                                      ) (i) ref


{-
toList :: BTree ref k v -> [(k, v)]
toList = fold f []
  where
    f t (Kv a b) = t ++ [(a, get b)]
-}

--new :: ( Monad m
--       , VNode.VNode t m k v
--       )
--    => m (BTree m t k v)
new = do
  n <- VecSTM.create
  return $ BTree 0 $ BLeafs n
