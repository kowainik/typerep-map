module Test.TypeRep.VectorOpt
    ( optimalVectorSpec
    ) where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.TypeRep.OptimalVector (TF (..), fromList, lookup)


-- | Simple test for 'lookup', 'insert' and 'size' functions.
optimalVectorSpec :: Spec
optimalVectorSpec = describe "Optimal Vector TypeRep" $
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [TF $ Identity 'a']) `shouldBe` Just (Identity 'a')
        it "returns the second inserted value of the same type" $
            lookup (fromList [TF (Identity 'b'), TF (Identity 'a')]) `shouldBe` Just (Identity 'b')
