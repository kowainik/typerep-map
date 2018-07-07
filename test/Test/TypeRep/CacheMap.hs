{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.TypeRep.CacheMap where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.TypeRep.CacheMap (TF (..), TypeRepMap, empty, fromList, insert, lookup, size)
import GHC.Fingerprint (Fingerprint (..))
import GHC.TypeLits
import Test.Tasty.Hspec (Spec, describe, example, it, shouldBe)

import Data.TypeRep.CacheMap (TF (..), TypeRepMap (..), cachedBinarySearch, fromList, lookup, size)

-- Simple test for 'lookup', 'insert' and 'size' functions.
spec_insertLookup :: Spec
spec_insertLookup = do
    describe "Lookup Test" $ do
        it "returns the inserted element" $
            lookup (fromList [TF $ Identity 'a']) `shouldBe` Just (Identity 'a')
        it "returns the inserted element" $
          lookup bigMap `shouldBe` Just (Proxy :: Proxy 4)
        it "size" $ example $ do
            print $ map fp $ buildBigMap 10 (Proxy :: Proxy 0) []
            putStrLn $ "index: ========  " ++ show (indexLookup @(Proxy 0) bigMap)

       -- it "returns the second inserted value of the same type" $
       --     lookup (fromList [TF (Identity 'b'), TF (Identity 'a')]) `shouldBe` Just (Identity 'b')

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

bigMap :: TypeRepMap (Proxy :: Nat -> *)
bigMap = fromList $ buildBigMap 10 (Proxy :: Proxy 0) []

buildBigMap :: forall a . (KnownNat a)
            => Int
            -> Proxy (a :: Nat)
            -> [TF (Proxy :: Nat -> *)]
            -> [TF (Proxy :: Nat -> *)]
buildBigMap 1 x = (TF x :)
buildBigMap n x = (TF x :) . buildBigMap (n - 1) (Proxy :: Proxy (a + 1))
fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy
fp :: TF f -> Fingerprint
fp (TF x) = typeRepFingerprint $ typeRep $ fromF x

indexLookup :: forall a f . Typeable a => TypeRepMap f -> Maybe Int
indexLookup tVect = cachedBinarySearch (typeRepFingerprint $ typeRep $ Proxy @a)
                                  (fingerprintAs tVect)
                                  (fingerprintBs tVect)
