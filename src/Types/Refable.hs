-- |

module Types.Refable where

import Types.Ref

class Refable a where
  makeRefIO :: a -> IO (Ref a)
  makeRef   :: a -> Ref a
  deRef :: (Ref a) -> a
