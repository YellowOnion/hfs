-- |

module Types.SortedLog where

import Data.Foldable
import Control.Lens

import Prelude hiding (sortOn)
import Types.VItem


class Foldable t => SortedLog t where
  append :: a -> t a -> t a
  sfoldr :: Ord a => (a -> b -> b) -> b -> t a -> b


instance SortedLog [] where
  append i ls = i : ls
  sfoldr f z = foldr f z . sortOn (^. _k)
