{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.TypeRep.MapProperty where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (Nat, SomeNat (..), someNatVal)
import Hedgehog (MonadGen, PropertyT, forAll, property, (===))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Data.TypeRep.CacheMap (TF (..), TypeRepMap, delete, fromList, insert, lookup, member)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type PropertyTest = [TestTree]

prop :: HasCallStack => TestName -> PropertyT IO () -> PropertyTest
prop testName = pure . testProperty testName . property

test_InsertLookup :: PropertyTest
test_InsertLookup =  prop "lookup k (insert k v m) == Just v" $ do
    m <- forAll genMap
    TF (proxy :: IntProxy n) <- forAll genTF

    lookup @n @IntProxy (insert proxy m) === Just proxy

test_InsertInsert :: PropertyTest
test_InsertInsert = prop "insert k b . insert k a == insert k b" $ do
    m <- forAll genMap
    TF a@(IntProxy (proxy :: Proxy n) i) <- forAll genTF
    let b = IntProxy proxy (i + 1)
    lookup @n @IntProxy (insert b $ insert a m) === Just b

test_DeleteMember :: PropertyTest
test_DeleteMember = prop "member k . delete k == False" $ do
    m <- forAll genMap
    TF (proxy :: IntProxy n) <- forAll genTF
    shouldInsert <- forAll Gen.bool

    if shouldInsert then
        member @n (delete @n $ insert proxy m) === False
    else
        member @n (delete @n m) === False

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

data IntProxy (n :: Nat) = IntProxy (Proxy n) Int
    deriving (Show, Eq)

genMap :: MonadGen m => m (TypeRepMap IntProxy)
genMap = fromList <$> Gen.list (Range.linear 0 1000) genTF

genTF :: MonadGen m => m (TF IntProxy)
genTF = do
    randNat :: Integer <- Gen.integral (Range.linear 0 10000)
    randInt <- Gen.int Range.constantBounded
    case someNatVal randNat of
        Just (SomeNat proxyNat) -> pure $ TF $ IntProxy proxyNat randInt
        Nothing                 -> error "Invalid test generator"
