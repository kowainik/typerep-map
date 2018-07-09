module Test.TypeRep.CacheMap where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import Data.TypeRep.CacheMap (TF (..), TypeRepMap, empty, fromList, insert, lookup, size, union)

-- Simple test for 'lookup', 'insert' and 'size' functions.
spec_insertLookup :: Spec
spec_insertLookup = do
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [TF $ Identity 'a']) `shouldBe` Just (Identity 'a')
        it "returns the second inserted value of the same type" $
            lookup (fromList [TF (Identity 'b'), TF (Identity 'a')]) `shouldBe` Just (Identity 'b')

    describe "Size Test" $ do
        it "is empty" $
            size empty `shouldBe` 0
        it "is of size 1 when 1 element inserted" $
            size (insert (Identity 'a') empty) `shouldBe` 1
        it "doesn't increase size when element of the same type is added" $
            size (insert (Identity 'b') $ insert (Identity 'a') empty) `shouldBe` 1
        it "returns 10 when 10 different types are inserted" $
            size mapOf10 `shouldBe` 10

    describe "Union test" $ do
        let m = fromList [TF $ Identity 'a', TF $ Identity True] `union` fromList [TF $ Identity 'b']
        it "lookup works on union as expected" $ do
            lookup m `shouldBe` Just (Identity 'a')
            lookup m `shouldBe` Just (Identity True)
            lookup @Int m `shouldBe` Nothing


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
