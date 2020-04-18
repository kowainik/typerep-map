module Test.TypeRep.CMap
    ( cMapSpec
    ) where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.TypeRep.CMap (TypeRepMap, empty, insert, lookup, size)


-- | Simple test for 'lookup', 'insert' and 'size' functions.
cMapSpec :: Spec
cMapSpec = describe "Containers Map TypeRep" $ do
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
        $ insert (Identity @String "aaa")
        $ insert (Identity $ Just 'a')
        $ insert (Identity 'a')
        $ insert (Identity (11 :: Int)) empty
