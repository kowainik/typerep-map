module Test.TypeRep.Map where

import Prelude hiding (lookup)

import Test.Tasty.Hspec

import Data.TypeRep.Map

-- Simple test for 'lookup', 'insert' and 'size' functions.
spec_insertLookup :: Spec
spec_insertLookup = do
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (insert 'a' empty) `shouldBe` Just 'a'
        it "returns the second inserted value of the same type" $
            lookup (insert 'b' $ insert 'a' empty) `shouldBe` Just 'b'

    describe "Size Test" $ do
        it "is empty" $
            size empty `shouldBe` 0
        it "is of size 1 when 1 element inserted" $
            size (insert 'a' empty) `shouldBe` 1
        it "doesn't increase size when element of the same type is added" $
            size (insert 'b' $ insert 'a' empty) `shouldBe` 1
        it "returns 10 when 10 different types are inserted" $
            size mapOf10 `shouldBe` 10


mapOf10 :: TypeRepMap
mapOf10 = insert True $ insert [True, False] $ insert (Just True)
        $ insert (Just ()) $ insert [()] $ insert ()
        $ insert "aaa" $ insert (Just 'a') $ insert 'a'
        $ insert (11 :: Int) empty
