module Test.TypeRep.Vector
    ( vectorSpec
    ) where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.TypeRep.Vector (TF (..), fromList, lookup)


-- | Simple test for 'lookup', 'insert' and 'size' functions.
vectorSpec :: Spec
vectorSpec = describe "Vector TypeRep" $
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [TF (Identity 'a')]) `shouldBe` Just (Identity 'a')
        it "returns the second inserted value of the same type" $
            lookup (fromList [TF (Identity 'b'), TF (Identity 'a')]) `shouldBe` Just (Identity 'b')
