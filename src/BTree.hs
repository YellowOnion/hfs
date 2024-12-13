{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}

module BTree where

import Debug.Trace

import Types.VNode qualified as VNode
import Control.Monad (forM_)

data BNode t k v where
  BNodes :: t k (BNode t k v) -> BNode t k v
  BLeafs :: t k v -> BNode t k v

data BTree m t k v where
  BTree :: { bFactor :: !Int
           , length :: !Int
           , bRoot :: !(BNode t k v)
           } -> BTree m t k v

lookup :: (Ord k)
       => k -> BTree m ref k v -> Maybe (Int, v)
lookup !k BTree{..} = go 0 bRoot
  where
    go !n (BLeafs ref) = error "not implemented"
    go !n (BNodes ref) = error "not implemented"

data NeedsSplit = LeafSplit | NodeSplit

insert :: (Monad m, VNode.VNode t m k v)
       => k
       -> v
       -> BTree m t k v
       -> m (BTree m t k v)
insert k v BTree{..} = do
  bt <- go (0 :: Int) bRoot
  case bt of
    Just bt -> return $ BTree bFactor (length + 1) bt
    Nothing -> error "Btree failed"
  where
 {--   go :: ( Monad m
          , VNode.VNode m t k v
          )
       => Int
       -> BNode m t k v
       -> m (BNode m t k v) -}
    go :: Monad m => Int -> BNode t k v -> m (Maybe (BNode t k v))
    go !n (BLeafs ref)
      | n > 64 = error "insert leaf too deep"
      | otherwise = do
          needsFlush <- VNode.appendLeaf k v ref
          case () of
            () | needsFlush && n == 0 -> do
                   leafs <- VNode.flushLeafs ref
                   let (lowerLeafs, upperLeafs) = splitAt bFactor leafs
                   lowerNode <- VNode.create bFactor
                   upperNode <- VNode.create bFactor
                   root    <- VNode.create bFactor
                   forM_ lowerLeafs (\(k, v) -> VNode.appendLeaf k v lowerNode)
                   forM_ upperLeafs (\(k, v) -> VNode.appendLeaf k v upperNode)
                   _ <- VNode.appendNode (fst $ last lowerLeafs) lowerNode root
                   _ <- VNode.appendNode (fst $ head upperLeafs) upperNode root
                   return . Just $ BNodes root
               | needsFlush -> return Nothing
               | otherwise -> return . Just $ BLeafs ref

    go !n (BNodes ref)
      | n > 64 = error "insert node too deep"
      | otherwise = do
        needsFlush <- VNode.appendLeaf k v ref
        case needsFlush of
          False -> return . Just $ BNodes ref
          True -> flush ref

        where flush = error "flush no!"

fold f init BTree{..} = go init bRoot
  where
    go a (BLeafs ref) = error "fold error"
    go a (BNodes ref) = error "fold error"

{-
toList :: BTree ref k v -> [(k, v)]
toList = fold f []
  where
    f t (Kv a b) = t ++ [(a, get b)]
-}

new :: ( Monad m
       , VNode.VNode t m k v
       )
    => m (BTree m t k v)
new = do
  n <- VNode.create 32
  return $ BTree 32 0 $ BLeafs n
