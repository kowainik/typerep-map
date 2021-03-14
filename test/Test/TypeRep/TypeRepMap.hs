module Test.TypeRep.TypeRepMap
    ( typeRepMapSpec
    ) where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Typeable (cast)
import GHC.Exts (fromList, toList)
import Type.Reflection (SomeTypeRep(..), typeRep)
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList, shouldSatisfy, expectationFailure)

import Data.TMap (TMap, empty, insert, lookup, one, size, union, keys)
import Data.TypeRepMap.Internal (WrapTypeable (..))


-- Simple test for 'lookup', 'insert', 'size', 'keys', 'toList' functions.
typeRepMapSpec :: Spec
typeRepMapSpec = describe "TypeRepMap" $ do
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [WrapTypeable $ Identity 'a']) `shouldBe` Just 'a'
        it "returns the second inserted value of the same type" $
            lookup (fromList [WrapTypeable (Identity 'b'), WrapTypeable (Identity 'a')]) `shouldBe` Just 'b'

    describe "Size Test" $ do
        it "is empty" $
            size empty `shouldBe` 0
        it "is of size 1 when 1 element inserted" $
            size (one 'a') `shouldBe` 1
        it "doesn't increase size when element of the same type is added" $
            size (insert 'b' $ insert 'a' empty) `shouldBe` 1
        it "returns 10 when 10 different types are inserted" $
            size mapOf10 `shouldBe` 10

    describe "Union test" $ do
        let m = fromList [WrapTypeable $ Identity 'a', WrapTypeable $ Identity True] `union`
                fromList [WrapTypeable $ Identity 'b']
        it "lookup works on union as expected" $ do
            lookup m `shouldBe` Just 'a'
            lookup m `shouldBe` Just True
            lookup @Int m `shouldBe` Nothing

    describe "Keys Test" $ do
      it "returns nothing on empty map" $
        keys empty `shouldBe` []
      it "returns the correct TypeRep" $
        keys (one 'a') `shouldBe` [SomeTypeRep $ typeRep @Char]
      it "returns the correct TypeReps for 10 different types" $
        keys mapOf10 `shouldMatchList`
          [ SomeTypeRep $ typeRep @Bool
          , SomeTypeRep $ typeRep @[Bool]
          , SomeTypeRep $ typeRep @(Maybe Bool)
          , SomeTypeRep $ typeRep @(Maybe ())
          , SomeTypeRep $ typeRep @[()]
          , SomeTypeRep $ typeRep @()
          , SomeTypeRep $ typeRep @String
          , SomeTypeRep $ typeRep @(Maybe Char)
          , SomeTypeRep $ typeRep @Char
          , SomeTypeRep $ typeRep @Int
          ]

    describe "ToList Test" $ do
      it "returns nothing on empty map" $
        toList empty `shouldSatisfy` null
      it "returns correct result when 1 element is inserted" $
        case toList (one 'a') of
          [WrapTypeable (Identity x)] -> cast x `shouldBe` Just 'a'
          _ -> expectationFailure "did not return exactly 1 result"
      it "returns correct result when 10 elements are inserted" $ do
        let
          getTypeRep (WrapTypeable (Identity (_ :: a))) = SomeTypeRep $ typeRep @a
          got = sortBy (comparing getTypeRep) (toList mapOf10)
          expected = sortBy (comparing fst)
            [ (SomeTypeRep $ typeRep @Bool,
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just True)
            , (SomeTypeRep $ typeRep @[Bool],
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just [True, False])
            , (SomeTypeRep $ typeRep @(Maybe Bool),
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just (Just True))
            , (SomeTypeRep $ typeRep @(Maybe ()),
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just (Just ()))
            , (SomeTypeRep $ typeRep @[()],
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just [()])
            , (SomeTypeRep $ typeRep @(),
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just ())
            , (SomeTypeRep $ typeRep @String,
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just ("aaa" :: String))
            , (SomeTypeRep $ typeRep @(Maybe Char),
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just (Just 'a'))
            , (SomeTypeRep $ typeRep @Char,
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just 'a')
            , (SomeTypeRep $ typeRep @Int,
              \(WrapTypeable (Identity x)) -> cast x `shouldBe` Just (11 :: Int))
            ]
        length got `shouldBe` 10
        sequence_ $ zipWith snd expected got

mapOf10 :: TMap
mapOf10 = insert True
        $ insert [True, False]
        $ insert (Just True)
        $ insert (Just ())
        $ insert [()]
        $ insert ()
        $ insert @String "aaa"
        $ insert (Just 'a')
        $ insert 'a'
        $ insert (11 :: Int) empty
