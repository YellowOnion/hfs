-- |

module Main where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)


import qualified List
import qualified Types.KvVector as KVec

{-
fromList :: [Int] -> Tree
fromList ls = foldl f tree ls
  where
    f :: Tree -> Int -> Tree
    f b k = BTree.insert k "" b
-}

type LList = List.List KVec.KvVector Int String

range = 1024
ascendingLs  = [0      ..  range-1] :: [Int]
descendingLs = [range-1,range-2..0] :: [Int]

main = hspec $ do
  describe "List basics" $ do
    it "nope" $ do
      a <- List.new
      return ()

{-
  describe "BTree basics" $ do

    it "make empty Btree" $ do
      return () -- BTree.new `shouldReturn` (BTree.BTree 32 0)
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
-}
