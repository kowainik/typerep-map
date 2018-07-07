{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver -fno-warn-orphans #-}

module DMap
       ( benchDMap
       , prepareBenchDMap
       ) where

import Criterion.Main (Benchmark, bench, bgroup, nf)

import Prelude hiding (lookup)

import Control.DeepSeq (rnf)
import Control.Exception
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits
import Type.Reflection (TypeRep, Typeable, typeRep)
import Type.Reflection.Unsafe (typeRepFingerprint)
import Unsafe.Coerce (unsafeCoerce)

import Data.Dependent.Map (DMap, empty, insert, keys, lookup)
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Some (Some (This))

benchDMap :: Benchmark
benchDMap = bgroup "dependent map"
   [ bench "lookup"     $ nf tenLookups bigMap
   -- , bench "insert new" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 9999999999)
   -- , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 1)
   ]

tenLookups :: DMap TypeRep Identity
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a :: Nat) . Typeable a => Proxy a
    lp = runIdentity $ fromJust $ lookup (typeRep @(Proxy a)) tmap

-- TypeRepMap of 10000 elements
bigMap :: DMap TypeRep Identity
bigMap = buildBigMap 10000 (Proxy :: Proxy 0) empty

buildBigMap :: forall a . (KnownNat a)
            => Int
            -> Proxy (a :: Nat)
            -> DMap TypeRep Identity
            -> DMap TypeRep Identity
buildBigMap 1 x = insert (typeRep @(Proxy a)) $ Identity x
buildBigMap n x = insert (typeRep @(Proxy a)) (Identity x)
                . buildBigMap (n - 1) (Proxy @(a + 1))

rknf :: DMap TypeRep f -> ()
rknf = rnf . map (\(This t) -> typeRepFingerprint t) . keys

prepareBenchDMap :: IO ()
prepareBenchDMap = evaluate (rknf bigMap)

instance GEq TypeRep where
    geq :: TypeRep a -> TypeRep b -> Maybe (a :~: b)
    geq (typeRepFingerprint -> a) (typeRepFingerprint -> b) =
        if a == b
            then Just $ unsafeCoerce Refl
            else Nothing

instance GCompare TypeRep where
    gcompare :: TypeRep a -> TypeRep b -> GOrdering a b
    gcompare (typeRepFingerprint -> a) (typeRepFingerprint -> b) =
        case compare a b of
            EQ -> unsafeCoerce GEQ
            LT -> GLT
            GT -> GGT
