{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BTree where

import Data.Vector qualified as Vector
import Data.Bifoldable
import Data.Bifunctor
import Debug.Trace

import Prelude hiding (length, lookup)

data Ref a = Ref {-# NOUNPACK #-} !a
  deriving Show

instance VRef Ref a where
  get (Ref a) = a
  {-# INLINE get #-}
  put !a = Ref a
  {-# INLINE put #-}


class VRef a b where
  get :: a b -> b
  put :: b -> a b


data Kv a b = Kv !a !b
  deriving (Show, Eq, Ord, Functor, Bifunctor)

kvKey (Kv k _) = k
kvValue (Kv _ v) = v


class ( Monad m
      , Bifunctor t
      , Bifoldable t
      ) => VNode m t k v
      | t -> k v, t -> m where
  -- create a node of fixed size
  create  :: Int -> m (t k v)
  -- append item and return true if full
  append  :: k -> v -> t k v -> m Bool
  destroy :: k -> t k v -> m ()

  split   :: t k v -> m (t k v, t k v)
  fuse    :: t k v -> t k v -> m ()

  size    :: t k v -> m Int

  find    :: k -> t k v -> m (Maybe (k, v))


data KvVector k v = KvVector (Vector.Vector (Kv k v))

newtype Key = Key
  { unKey :: Int }

newtype KeyRef = KeyRef
  { unRef :: Int }

deriving instance Functor (KvVector k)
deriving instance Bifunctor KvVector
deriving instance Bifoldable KvVector
deriving instance VNode IO KvVector Int Int


type VNodes n ref k v = Vector.Vector (Kv k (n ref k v))
type VLeafs ref k v = Vector.Vector (Kv k (ref v))


data BNode t k v where
  BNodes :: t k (BNode t k v) -> BNode t k v
  BLeafs :: t k v -> BNode t k v

instance ( Show k
         , Show v
         , Show (Ref v)
         )
        => Show (BNode Ref k v) where
  show (BNodes ref) = "Nodes: " ++ show ref
  show (BLeafs ref) = "Leafs: " ++ show ref

data BTree ref k v where
  BTree :: { bFactor :: !Int
           , length :: !Int
           , bRoot :: !(BNode ref k v)
           } -> BTree ref k v

instance ( Show k
         , Show v
         ) => Show (BTree Ref k v) where
  show BTree{..} = "BTree size: " ++ show length ++ "; " ++ show bRoot

lookup :: ( VRef ref v
          , VRef ref (VNodes BNode ref k v) -- Interior nodes
          , VRef ref (VLeafs ref k v) -- Leafs
          , Ord k)
       => k -> BTree ref k v -> Maybe (Int, v)
lookup !k BTree{..} = go 0 bRoot
  where
    go !n (BLeafs ref) =
      case Vector.takeWhile ((<= k) . kvKey) leafs of
        rest | Vector.null rest -> Nothing
             | otherwise        -> Just . (n,) . get . kvValue $ Vector.last rest

      where
        leafs = get ref
    go !n (BNodes ref) =
      case Vector.takeWhile ((<= k) . kvKey) nodes of
        rest | Vector.null rest -> Nothing
             | otherwise        -> go (n+1) . kvValue $ Vector.last rest
      where
        nodes = get ref

data NeedsSplit = LeafSplit | NodeSplit

insert :: ( Ord k
          , Show k
          , VRef ref v
          , VRef ref (VLeafs ref k v)
          , VRef ref (VNodes BNode ref k v))
       => k -> v -> BTree ref k v -> BTree ref k v
insert k v BTree{..} = BTree bFactor (length + 1)
  $! case go (0 :: Int) bRoot of
        Left LeafSplit -> error "BTree leaf is full???"
        Left NodeSplit -> error "BTree node is full???"
        Right a -> a
  where
    singleton = Vector.singleton $! Kv k (put v)

    split a = case a of
      BLeafs ref -> split' BLeafs ref
      BNodes ref -> split' BNodes ref
      where
        split' t ref = BNodes . put $  Vector.fromList
                       [ Kv (kvKey $ Vector.last lower) . t $ put lower
                       , Kv (kvKey $ Vector.head upper) . t $ put upper ]
          where
            nodes = get ref
            (lower, upper) = Vector.splitAt bFactor nodes

    go !n l@(BLeafs ref)
      | n > 64 = error $ "insert leaf too deep: " ++ show k
      | Vector.null leafs = Right . BLeafs $! put singleton
      | Vector.length leafs >= bFactor * 2 - 1 = if n == 0
                                                 then go n $ split l
                                                 else Left LeafSplit
      | otherwise = Right . BLeafs . put $! lower' Vector.++ singleton Vector.++ upper'
      where
        leafs = get ref
        (lower', upper') = Vector.partition ((< k) . kvKey) leafs

    go !n (BNodes ref)
      | n > 64 = error $ "insert node too deep: " ++ show k
      | Vector.null nodes && n > 0 = error "non-root node should not be empty"
--      | Vector.null nodes = Just . BLeafs $! put singleton
      | otherwise = a
      where
        nodes = get ref

        uhh f1 (Kv k_ v_) = case go n' v_ of
          Right v' -> f2 (Kv k_ v')
          Left _ -> uhh f1 $ Kv k_ $ split v_
          where
            f2 = Right . BNodes . put . f1

        a = case (Vector.unsnoc lower, Vector.uncons upper) of
                (Nothing, Nothing) -> error "Node empty, but should have items"
                (Nothing, Just (upperHead, upperTail)) -> uhh (\i -> Vector.cons i upperTail) upperHead
                (Just (lowerInit, lowerLast), Nothing) -> uhh (Vector.snoc lowerInit) lowerLast
                (Just (lowerInit, lowerLast), Just _ ) -> uhh (\i -> Vector.snoc lowerInit i Vector.++ upper) lowerLast

        n' = n + 1
        (lower, upper) = Vector.partition ((<= k) . kvKey) nodes


fold :: ( VRef ref (VLeafs ref k v)
        , VRef ref (VNodes BNode ref k v))
     => (t -> Kv k (ref v) -> t) -> t -> BTree ref k v -> t
fold f init BTree{..} = go init bRoot
  where
    go a (BLeafs ref) = Vector.foldl' (f) a leafs
      where leafs = get ref
    go a (BNodes ref) = Vector.foldl' (\a -> go a . kvValue) a nodes
      where nodes = get ref


toList :: ( VRef ref (VLeafs ref k v)
          , VRef ref (VNodes BNode ref k v)
          , VRef ref v
          )
       => BTree ref k v -> [(k, v)]
toList = fold f []
  where
    f t (Kv a b) = t ++ [(a, get b)]


empty :: ( VRef ref v
         , VRef ref (VLeafs ref k v)
         ) => BTree ref k v
empty = BTree 32 0 (BLeafs . put $ Vector.empty)
