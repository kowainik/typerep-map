module Test.TypeRep.CMap where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))

import Test.Tasty.Hspec

import Data.TypeRep.CMap

-- Simple test for 'lookup', 'insert' and 'size' functions.
spec_insertLookup :: Spec
spec_insertLookup = do
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (insert (Identity 'a') empty) `shouldBe` Just (Identity 'a')
        it "returns the second inserted value of the same type" $
            lookup (insert (Identity 'b') $ insert (Identity 'a') empty) `shouldBe` Just (Identity 'b')

    describe "Size Test" $ do
        it "is empty" $
            size empty `shouldBe` 0
        it "is of size 1 when 1 element inserted" $
            size (insert (Identity 'a') empty) `shouldBe` 1
        it "doesn't increase size when element of the same type is added" $
            size (insert (Identity 'b') $ insert (Identity 'a') empty) `shouldBe` 1
        it "returns 10 when 10 different types are inserted" $
            size mapOf10 `shouldBe` 10


mapOf10 :: TypeRepMap Identity
mapOf10 = insert (Identity True)
        $ insert (Identity [True, False])
        $ insert (Identity $ Just True)
        $ insert (Identity $ Just ())
        $ insert (Identity [()])
        $ insert (Identity ())
        $ insert (Identity "aaa")
        $ insert (Identity $ Just 'a')
        $ insert (Identity 'a')
        $ insert (Identity (11 :: Int)) empty
