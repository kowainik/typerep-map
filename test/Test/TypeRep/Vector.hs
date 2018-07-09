module Test.TypeRep.Vector where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))

import Test.Tasty.Hspec

import Data.TypeRep.Vector

-- Simple test for 'lookup', 'insert' and 'size' functions.
spec_insertLookup :: Spec
spec_insertLookup =
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [TF (Identity 'a')]) `shouldBe` Just (Identity 'a')
        it "returns the second inserted value of the same type" $
            lookup (fromList [TF (Identity 'b'), TF (Identity 'a')]) `shouldBe` Just (Identity 'b')
