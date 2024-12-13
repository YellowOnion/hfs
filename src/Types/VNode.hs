{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |

module Types.VNode where

-- This is meant to be node inspired by fractal trees
-- t is the node
-- m is the monad it sits in
-- k is the key
-- iv is the value that points to other nodes in interior nodes
-- v is the value that points to exterior data

class ( Monad m
      , Ord k
      ) => VNode t m k v
      | t -> m
       where
  -- create a node of fixed size
  create  :: Int -> m (t k v)
  delete  :: t k v -> m ()
  -- append item and return true if full
  appendNode  :: Ord k => k -> iv -> t k v -> m Bool
  appendLeaf  :: k -> v -> t k v -> m Bool
  destroyNode :: k -> t k v -> m ()
  destroyLeaf :: k -> t k v -> m ()

  split  :: t k v -> m (t k v, t k v)
  flushLeafs  :: t k v -> m [(k, v)]
  fuse    :: t k v -> t k v -> m (t k v)

  size    :: t k v-> m Int

  findNode :: k -> t k v -> m (Maybe (k, iv))
  findLeaf :: k -> t k v -> m (Maybe (k, v))
