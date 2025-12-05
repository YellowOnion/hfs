module Main where

import GHC.Clock
import Data.Vector qualified as Vector

import Debug.Trace
import Prelude hiding (length, lookup)

import Control.Concurrent.STM
import Control.Monad (foldM)

import Backend.Bucket

badHash :: Int -> Int
badHash n = (p * (n ^ 2 + 1)) `mod` 2^8
  where
    p = 2^255 - 19

main :: IO ()
main = do
  putStrLn "formatting..."
  _h <- formatStore "./data/store"
  putStrLn "done."
  putStrLn "opening..."
  store <- openStore2 _h
  putStrLn "done."
  --printStore store
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
