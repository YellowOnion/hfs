{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module List where

import Types.VNode qualified as VNode
import Types.VNode (KeyType, RunVNode)


data List s k v = Nil (s k v)
  | Cons (s k (List s k v))

type instance VNode.KeyType (List s k v) = k
type instance VNode.ValueType (List s k v) = v

type VNode' s k v r = VNode.VNode (List s k v) (s k v) k r

create :: (KeyType (List s k v) ~ KeyType (s k v))
        => VNode' s k v (s k v)
create = VNode.Create

delete :: (KeyType (List s k v) ~ KeyType (s k v)) => s k v -> VNode' s k v ()
delete = VNode.Delete

size  :: s k v -> VNode' s k v Int
size  = VNode.Size

length :: s k v -> VNode' s k v Int
length = VNode.Length

append :: k -> VNode.ValueType (s k v) -> s k v -> VNode' s k v Bool
append = VNode.Append

remove :: k -> s k v -> VNode' s k v ()
remove = VNode.Remove

_fold :: (VNode.ValueType (s k v) ~ v) =>
  (b -> VNode.VItem k v -> b)
                  -> b -> s k v -> VNode' s k v b
_fold = VNode.Fold

new :: ( Monad m
       , RunVNode m s
       , KeyType (s k v) ~ k
       , KeyType (List s k v) ~ k
       )
    => m (List s k v)
new = do
  n <- VNode.runVNode create
  return $ Nil n
