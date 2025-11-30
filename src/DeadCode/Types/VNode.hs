{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- |


module Types.VNode where

import Data.Kind

type family KeyType k
type family ValueType v
type family StorageType s :: * -> * -> *


-- t is the abstract data structure like a List or BTree
-- s is the storage backend for this node like a file block
-- m is the monad that s needs to be run inside like IO
-- t and s should not care about each other beyond k and v
-- s needs to know k and v to serialise them
-- the user should be able to specify t, s k and v

data VNode t s k a where
  -- Create a Node to be serialized to disk
  Create     :: (KeyType t ~ KeyType s, KeyType t ~ k) => VNode t s k s
  -- Detele Node -- maybe use garbage collection??
  Delete     :: (KeyType t ~ KeyType s, KeyType t ~ k) => s -> VNode t s k ()

  -- return the max size of the Node
  -- TODO Varible length nodes??
  Size       :: s -> VNode t s k Int
  Length     :: s -> VNode t s k Int

  Append :: KeyType t -> ValueType s -> s -> VNode t s k Bool

  Remove :: KeyType t -> s -> VNode t s k ()

  -- Probably don't want the input s to constrain the output s type?
  Fold   :: (b -> VItem (KeyType t) (ValueType s) -> b) -> b -> s -> VNode t s k b


  Pure :: a -> VNode t s k a
  Bind :: VNode t s k a -> (a -> VNode t s k b) -> VNode t s k b

instance Functor (VNode t k s) where
  fmap f g = Bind g (Pure . f)

instance Applicative (VNode t k s) where
  pure = Pure
  f <*> g = Bind f (<$> g)

instance Monad (VNode t k s) where
  (>>=) = Bind


class RunVNode m s where
  runVNode :: (Monad m, KeyType (t s k v) ~ KeyType (s k v), KeyType (s k v) ~ k) => VNode (t s k v) (s k v) k a -> m a
