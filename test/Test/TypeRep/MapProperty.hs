{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.TypeRep.MapProperty where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Exts (fromList)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (Nat, SomeNat (..), someNatVal)
import Hedgehog (MonadGen, PropertyT, forAll, property, (===), assert)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Data.TypeRepMap.Internal (TypeRepMap (..), WrapTypeable (..), delete, insert, lookup, member, invariantCheck)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

----------------------------------------------------------------------------
-- Common utils
----------------------------------------------------------------------------

type PropertyTest = [TestTree]

prop :: HasCallStack => TestName -> PropertyT IO () -> PropertyTest
prop testName = pure . testProperty testName . property

----------------------------------------------------------------------------
-- Map modification properties
----------------------------------------------------------------------------

test_InsertLookup :: PropertyTest
test_InsertLookup =  prop "lookup k (insert k v m) == Just v" $ do
    m <- forAll genMap
    WrapTypeable (proxy :: IntProxy n) <- forAll genTF
    lookup @n @IntProxy (insert proxy m) === Just proxy

test_InsertInsert :: PropertyTest
test_InsertInsert = prop "insert k b . insert k a == insert k b" $ do
    m <- forAll genMap
    WrapTypeable a@(IntProxy (proxy :: Proxy n) i) <- forAll genTF
    let b = IntProxy proxy (i + 1)
    lookup @n @IntProxy (insert b $ insert a m) === Just b

test_DeleteMember :: PropertyTest
test_DeleteMember = prop "member k . delete k == False" $ do
    m <- forAll genMap
    WrapTypeable (proxy :: IntProxy n) <- forAll genTF
    shouldInsert <- forAll Gen.bool

    if shouldInsert then
        member @n (delete @n $ insert proxy m) === False
    else
        member @n (delete @n m) === False

test_InsertInvariant :: PropertyTest
test_InsertInvariant = prop "invariantCheck (insert k b) == True" $ do
    m <- forAll genMap
    WrapTypeable a <- forAll genTF
    assert $ invariantCheck (insert a m)

test_DeleteInvariant :: PropertyTest
test_DeleteInvariant = prop "invariantCheck (delete k b) == True" $ do
    m <- forAll genMap
    WrapTypeable (_ :: IntProxy n) <- forAll genTF
    assert $ invariantCheck (delete @n m)

----------------------------------------------------------------------------
-- Semigroup and Monoid laws
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 806
-- This newtype is used to compare 'TypeRepMap's using only 'Fingerprint's. It's
-- not a good idea to write such `Eq` instance for `TypeRepMap` itself because
-- it doesn't compare values so it's not true equality. But this should be
-- enough for tests.
newtype FpMap f = FpMap (TypeRepMap f)
  deriving stock (Show, Semigroup, Monoid)

instance Eq (FpMap f) where
    FpMap (TypeRepMap as1 bs1 _ _) == FpMap (TypeRepMap as2 bs2 _ _) =
        as1 == as2 && bs1 == bs2
#endif

test_SemigroupAssoc :: PropertyTest
test_SemigroupAssoc = prop "x <> (y <> z) == (x <> y) <> z" $ do
#if __GLASGOW_HASKELL__ >= 806
    x <- forAll genMap
    y <- forAll genMap
    z <- forAll genMap
#else
    x <- FpMap <$> forAll genMap
    y <- FpMap <$> forAll genMap
    z <- FpMap <$> forAll genMap
#endif

    (x <> (y <> z)) === ((x <> y) <> z)

test_MonoidIdentity :: PropertyTest
test_MonoidIdentity = prop "x <> mempty == mempty <> x == x" $ do
#if __GLASGOW_HASKELL__ >= 806
    x <- forAll genMap
#else
    x <- FpMap <$> forAll genMap
#endif

    x <> mempty === x
    mempty <> x === x

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

data IntProxy (n :: Nat) = IntProxy (Proxy n) Int
    deriving stock (Show, Eq)

genMap :: MonadGen m => m (TypeRepMap IntProxy)
genMap = fromList <$> Gen.list (Range.linear 0 1000) genTF

genTF :: MonadGen m => m (WrapTypeable IntProxy)
genTF = do
    randNat :: Integer <- Gen.integral (Range.linear 0 10000)
    randInt <- Gen.int Range.constantBounded
    case someNatVal randNat of
        Just (SomeNat proxyNat) -> pure $ WrapTypeable $ IntProxy proxyNat randInt
        Nothing                 -> error "Invalid test generator"
