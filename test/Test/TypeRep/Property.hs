{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.TypeRep.Property where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (Nat, SomeNat (..), someNatVal)
import Hedgehog (MonadGen, PropertyT, forAll, property, (===))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Data.TypeRep.CacheMap (TF (..), TypeRepMap, fromList, insert, lookup)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type PropertyTest = [TestTree]

prop :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
prop testName = pure . testProperty testName . property

test_InsertLookup :: PropertyTest
test_InsertLookup =  prop "lookup k (insert k v m) == Just v" $ do
    m <- forAll genMap
    TF (proxy :: Proxy n) <- forAll genTF

    lookup @n @Proxy (insert proxy m) === Just proxy


genMap :: MonadGen m => m (TypeRepMap (Proxy :: Nat -> *))
genMap = fromList <$> Gen.list (Range.linear 0 1000) genTF

genTF :: MonadGen m => m (TF (Proxy :: Nat -> *))
genTF = do
    randInteger :: Integer <- Gen.integral (Range.linear 0 10000)
    case someNatVal randInteger of
        Just (SomeNat proxyNat) -> pure $ TF proxyNat
        Nothing                 -> error "Invalid test generator"
