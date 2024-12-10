module Main where

import Data.Vector qualified as Vector

import Debug.Trace

data Ref a = Ref a
  deriving Show

instance Show a => VRef Ref a where
  get (Ref a) = a
  put = Ref


class VRef a b where
  get :: a b -> b
  put :: b -> a b


type VNodes n ref k v = (Vector.Vector (k, n ref k v))
type VLeafs ref k v = (Vector.Vector (k, ref v))

data BNode ref k v = BNodes (ref (VNodes BNode ref k v))
                   | BLeafs (ref (VLeafs ref k v))

instance ( Show k
         , Show v
         , Show (Ref v)
         )
        => Show (BNode Ref k v) where
  show (BNodes ref) = "Nodes: " ++ show ref
  show (BLeafs ref) = "Leafs: " ++ show ref

data BTree ref k v = BTree
  { bFactor :: Int
  , bRoot :: BNode ref k v
  }

instance (Show k, Show v) => Show (BTree Ref k v) where
  show (BTree _ r ) = "BTree: " ++ show r

lookup :: ( VRef ref v
          , VRef ref (VNodes BNode ref k v) -- Interior nodes
          , VRef ref (VLeafs ref k v) -- Leafs
          , Ord k)
       => k -> BTree ref k v -> Maybe v
lookup k BTree{..} = go bRoot
  where
    go (BLeafs ref) =
      case Vector.takeWhile ((<= k) . fst) leafs of
        rest | Vector.null rest -> Nothing
             | otherwise        -> Just . get . snd $ Vector.last rest

      where
        leafs = get ref
    go (BNodes ref) =
      case Vector.takeWhile ((<= k) . fst) nodes of
        rest | Vector.null rest -> Nothing
             | otherwise        -> go . snd $ Vector.last rest
      where
        nodes = get ref


insert :: ( Ord k
          , Show k
          , VRef ref v
          , VRef ref (VLeafs ref k v)
          , VRef ref (VNodes BNode ref k v))
       => k -> v -> BTree ref k v -> BTree ref k v
insert k v BTree{..} = BTree bFactor $ go (0 :: Int) bRoot
  where
    singleton = Vector.singleton (k, put v)

    go n (BLeafs ref)
      | n > 512 = error $ "insert leaf too deep: " ++ show k
      | Vector.null leafs = BLeafs $ put singleton
      | Vector.length leafs >= bFactor * 2 = go (n+1) $
        BNodes . put $ Vector.fromList
        [ (lowerKey, BLeafs $ put lower), (upperKey, BLeafs $  put upper)]
      | otherwise = BLeafs . put $ lower' Vector.++ singleton Vector.++ upper'
      where
        leafs = get ref
        (lower, upper) = Vector.splitAt bFactor leafs
        (lower', upper') = Vector.partition ((< k) . fst) leafs
        lowerKey = fst $ Vector.head lower
        upperKey = fst $ Vector.head upper

    go n (BNodes ref)
      | n > 10 = error $ "insert node too deep: " ++ show k
      | Vector.null nodes && n > 0 = error "non-root node should not be empty"
      | Vector.null nodes = BLeafs $ put singleton
      | Vector.length nodes >= bFactor * 2 = go (n+1) $
        BNodes . put $ Vector.fromList
          [(lowerKey, BNodes $ put lower), (upperKey, BNodes $ put upper)]
      | otherwise = BNodes . put $ lower'' Vector.++ upper'
      where
        -- TODO figure out < or <=
        nodes = get ref
        (lower, upper) = Vector.splitAt bFactor nodes
        (lower', upper') = Vector.partition ((< k) . fst) nodes
        lower'' = case Vector.unsnoc lower' of
          Just (vs, i) -> vs `Vector.snoc` (max (fst i) k, go (n+1) $ snd i )
          Nothing -> Vector.singleton (k, BLeafs $ put singleton)

        lowerKey = fst $ Vector.head lower
        upperKey = fst $ Vector.head upper

empty :: ( VRef ref v
         , VRef ref (VLeafs ref k v)
         ) => BTree ref k v
empty = BTree 4 (BLeafs . put $ Vector.empty)

tree :: BTree Ref Int String
tree = empty

badHash :: Int -> Int
badHash n = (p * (n ^ 2 + 1)) `mod` 2^8
  where
    p = 2^255 - 19

testIO :: [Int] -> IO ()
testIO ls = do
  print ls
  print $ foldr (\k -> insert k "" . traceShowId) tree ls

main :: IO ()
main = do
  mapM_ testIO [map badHash [2^8..2^8+10]
               , [0..10]
               , [10,9..0]
               ]
