{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.TypeRep.TypeRepMapProperty
    ( typeRepMapPropertySpec
    ) where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Exts (fromList)
import GHC.TypeLits (Nat, SomeNat (..), someNatVal)
import Hedgehog (MonadGen, assert, forAll, (===))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Data.TypeRepMap.Internal (TypeRepMap (..), WrapTypeable (..), delete, insert, invariantCheck,
                                 lookup, member, generateOrderMapping, fromSortedList, alter,
                                 adjust)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


typeRepMapPropertySpec :: Spec
typeRepMapPropertySpec = describe "TypeRepMap Property tests" $ do
    describe "Map modification properties" $ do
        insertLookupSpec
        insertInsertSpec
        deleteMemberSpec
        insertInvariantSpec
        deleteInvariantSpec
        alterInsertSpec
        alterDeleteSpec
        alterAdjustSpec
        alterModifySpec
    describe "Internal helpers" $ do
        generateOrderMappingInvariantSpec
    describe "Instance Laws" $ do
        semigroupAssocSpec
        monoidIdentitySpec

----------------------------------------------------------------------------
-- Map modification properties
----------------------------------------------------------------------------

type Property = SpecWith (Arg Expectation)

insertLookupSpec :: Property
insertLookupSpec = it "lookup k (insert k v m) == Just v" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable (proxy :: IntProxy n) <- forAll genTF
    lookup @n @IntProxy (insert proxy m) === Just proxy

insertInsertSpec :: Property
insertInsertSpec = it "insert k b . insert k a == insert k b" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable a@(IntProxy (proxy :: Proxy n) i) <- forAll genTF
    let b = IntProxy proxy (i + 1)
    lookup @n @IntProxy (insert b $ insert a m) === Just b

deleteMemberSpec :: Property
deleteMemberSpec = it "member k . delete k == False" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable (proxy :: IntProxy n) <- forAll genTF
    shouldInsert <- forAll Gen.bool

    if shouldInsert then
        member @n (delete @n $ insert proxy m) === False
    else
        member @n (delete @n m) === False

insertInvariantSpec :: Property
insertInvariantSpec = it "invariantCheck (insert k b) == True" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable a <- forAll genTF
    assert $ invariantCheck (insert a m)

deleteInvariantSpec :: Property
deleteInvariantSpec = it "invariantCheck (delete k b) == True" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable (_ :: IntProxy n) <- forAll genTF
    assert $ invariantCheck (delete @n m)

alterInsertSpec :: Property
alterInsertSpec = it "insert proxy m === alter (const (Just proxy)) m" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable (proxy :: IntProxy n) <- forAll genTF
    insert proxy m === alter (const (Just proxy)) m

alterDeleteSpec :: Property
alterDeleteSpec = it "delete proxy m === alter (const Nothing) m" $ hedgehog $ do
    WrapTypeable (proxy :: IntProxy n) <- forAll genTF
    m <- insert proxy <$> forAll genMap
    delete @n @IntProxy m === alter @n @IntProxy (const Nothing) m

alterAdjustSpec :: Property
alterAdjustSpec = it "adjust f m == alter (fmap f) m" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable (_ :: IntProxy n) <- forAll genTF
    randInt <- forAll (Gen.int Range.constantBounded)
    let f (IntProxy p n) = IntProxy p (n * 10)
    adjust @n @IntProxy f m === alter @n @IntProxy (fmap f) m

alterModifySpec :: Property
alterModifySpec = it "lookup k (alter f) == f (lookup k m)" $ hedgehog $ do
    m <- forAll genMap
    WrapTypeable (_ :: IntProxy n) <- forAll genTF
    randInt <- forAll (Gen.int Range.constantBounded)
    -- Function with some interesting behaviour, which inserts, seletes and modifies
    let f Nothing = Just (IntProxy Proxy randInt)
        f (Just (IntProxy p n))
          | even n = Nothing
          | otherwise = Just $ IntProxy p (n * 10)
    lookup @n @IntProxy (alter @n f m) === f (lookup @n @IntProxy m)

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------
generateOrderMappingInvariantSpec :: Property
generateOrderMappingInvariantSpec = 
    it "fmap (fromSortedList [1 .. n] !!) (generateOrderMapping n) == [1 .. n]" $ hedgehog $ do
        n <- forAll $ Gen.int (Range.linear 0 100)
        fmap (fromSortedList [1 .. n] !!) (generateOrderMapping n) === [1 .. n]

----------------------------------------------------------------------------
-- Semigroup and Monoid laws
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 806
{- | This newtype is used to compare 'TypeRepMap's using only 'Fingerprint's.
It's not a good idea to write such 'Eq' instance for 'TypeRepMap' itself because
it doesn't compare values so it's not true equality. But this should be enough
for tests.
-}
newtype FpMap f = FpMap (TypeRepMap f)
    deriving newtype (Show, Semigroup, Monoid)

instance Eq (FpMap f) where
    FpMap (TypeRepMap as1 bs1 _ _) == FpMap (TypeRepMap as2 bs2 _ _) =
        as1 == as2 && bs1 == bs2
#endif

semigroupAssocSpec :: Property
semigroupAssocSpec = it "x <> (y <> z) == (x <> y) <> z" $ hedgehog $ do
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

monoidIdentitySpec :: Property
monoidIdentitySpec = it "x <> mempty == mempty <> x == x" $ hedgehog $ do
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
