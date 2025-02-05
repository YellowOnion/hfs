{-# LANGUAGE TemplateHaskell #-}
-- |

module Types.VItem where

import Control.Lens

data VItem k v
  = VItem !k !v
  | VItemDel !k
  deriving (Show, Eq)

_k :: Lens (VItem k v) (VItem k' v) k k'
_k = lens getter setter
  where
    getter (VItem k _)    = k
    getter (VItemDel k)   = k
    setter (VItem _ v) k' = VItem k' v
    setter (VItemDel _) k' = VItemDel k'

_v :: Traversal (VItem k v) (VItem k v') v v'
_v f (VItem k v)  = VItem k <$> f v
_v _ (VItemDel k) = pure (VItemDel k)
