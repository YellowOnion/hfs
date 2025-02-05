module Main where

import GHC.Clock
import Data.Vector qualified as Vector

import Debug.Trace
import Prelude hiding (length, lookup)

import Control.Concurrent.STM
import Control.Monad (foldM)

import BTree qualified

badHash :: Int -> Int
badHash n = (p * (n ^ 2 + 1)) `mod` 2^8
  where
    p = 2^255 - 19

testIO :: [Int] -> IO ()
testIO ls = do
  print ls
  tree :: BTree.BTree STM Int String <- atomically $ BTree.new
  tree <- atomically $ foldM (\t k -> BTree.insert k "" t) tree ls
  m <- atomically $ BTree.foldn (\n m kv -> m >> print (n, kv)) (return ()) tree
  m

main :: IO ()
main = do
  mapM_ testIO [map badHash [2^8..2^8+10]
               , [0..10]
               , [10,9..0]
               , [0..36]
               , [0..300]
               ]

{-
  putStrLn "BIG"
  t1 <- getMonotonicTime

  let tr = foldr ((`BTree.insert` "") . badHash) tree [0..2^20]
  t2 <- tr `seq` getMonotonicTime
  print $ round $ (t2 - t1) * 1000
  let c = BTree.fold (\i _ -> i + 1) 0 tr
  t3 <- c `seq` getMonotonicTime
  putStrLn $ "items in btree: " ++ show c
  print $ round $ (t3 - t2) * 1000
  t4 <- getMonotonicTime
  case BTree.lookup (2^14) tr of
    Nothing -> putStrLn "Can't find item!"
    Just item -> do
      t5 <- item `seq` getMonotonicTime
      putStrLn $ "depth: " ++ show (fst item)
      print $ round $ (t5 - t4) * 1000
-}
