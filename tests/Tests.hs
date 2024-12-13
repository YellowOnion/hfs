-- |

module Main where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)


import BTree qualified


type Tree = BTree.BTree IO KvVector Int String


fromList :: [Int] -> Tree
fromList ls = foldl f tree ls
  where
    f :: Tree -> Int -> Tree
    f b k = BTree.insert k "" b

range = 1024
ascendingLs  = [0      ..  range-1] :: [Int]
descendingLs = [range-1,range-2..0] :: [Int]

main = hspec $ do
  let

  describe "BTree basics" $ do

    it "make empty Btree" $ do
      return ()
      -- Btree.empty :: IO Tree

    it "check inserts / lookup" $ do
      return ()
      -- (BTree.lookup 0 $ BTree.insert 0 "" tree) `shouldBe` (Just (0 :: Int, ""))

  describe "BTree bulk tests" $ do

    it "ascending order inserts" $ do
      return ()
      -- (BTree.toList $ fromList ascendingLs) `shouldBe` map (,"") ascendingLs

    it "descending order inserts" $ do
      return ()
      -- (BTree.toList $ fromList descendingLs) `shouldBe` map (,"") ascendingLs
