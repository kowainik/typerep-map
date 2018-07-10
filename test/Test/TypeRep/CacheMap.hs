module Test.TypeRep.CacheMap where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import Data.TypeRep.Map (TF (..), fromList)
import Data.TypeRep.TMap (TMap, insert, lookup, one, size, union)

-- Simple test for 'lookup', 'insert' and 'size' functions.
spec_insertLookup :: Spec
spec_insertLookup = do
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [TF $ Identity 'a']) `shouldBe` Just 'a'
        it "returns the second inserted value of the same type" $
            lookup (fromList [TF (Identity 'b'), TF (Identity 'a')]) `shouldBe` Just 'b'

    describe "Size Test" $ do
        it "is empty" $
            size mempty `shouldBe` 0
        it "is of size 1 when 1 element inserted" $
            size (one 'a') `shouldBe` 1
        it "doesn't increase size when element of the same type is added" $
            size (insert 'b' $ insert 'a' mempty) `shouldBe` 1
        it "returns 10 when 10 different types are inserted" $
            size mapOf10 `shouldBe` 10

    describe "Union test" $ do
        let m = fromList [TF $ Identity 'a', TF $ Identity True] `union` fromList [TF $ Identity 'b']
        it "lookup works on union as expected" $ do
            lookup m `shouldBe` Just 'a'
            lookup m `shouldBe` Just True
            lookup @Int m `shouldBe` Nothing


mapOf10 :: TMap
mapOf10 = insert True
        $ insert [True, False]
        $ insert (Just True)
        $ insert (Just ())
        $ insert [()]
        $ insert ()
        $ insert "aaa"
        $ insert (Just 'a')
        $ insert 'a'
        $ insert (11 :: Int) mempty
