{-# LANGUAGE BangPatterns         #-}
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
       ) where

import Criterion.Main (Benchmark, bench, bgroup, nf, whnf, env)

import Prelude hiding (lookup)

import Control.DeepSeq (NFData(..))
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

type TypeRepMap = DMap TypeRep

benchDMap :: Benchmark
benchDMap = 
   env mkBigMap $ \ ~(Hack bigMap) ->
     bgroup "dependent map"
       [ bench "lookup"     $ nf tenLookups bigMap
       , bench "insert new 10 elements" $ whnf (inserts empty 10) (Proxy :: Proxy 0)
       , bench "insert big 1 element" $ whnf (inserts bigMap 1) (Proxy :: Proxy 0)
       -- , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 1)
       ]

tenLookups :: TypeRepMap (Proxy :: Nat -> *)
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a :: Nat) . Typeable a => Proxy a
    lp = fromJust $ lookup (typeRep @a) tmap

inserts :: forall a . (KnownNat a)
        => TypeRepMap (Proxy :: Nat -> *)
        -> Int
        -> Proxy (a :: Nat)
        -> TypeRepMap (Proxy :: Nat -> *)
inserts !c 0 _ = c
inserts !c n x = inserts
   (insert (typeRep @ a) x c)
   (n-1)
   (Proxy :: Proxy (a+1))

-- TypeRepMap of 10000 elements
mkBigMap :: IO (Hack (Proxy :: Nat -> *))
mkBigMap = pure . Hack $ buildBigMap 10000 (Proxy :: Proxy 0) empty

buildBigMap :: forall a . (KnownNat a)
            => Int
            -> Proxy (a :: Nat)
            -> TypeRepMap (Proxy :: Nat -> *)
            -> TypeRepMap (Proxy :: Nat -> *)
buildBigMap 1 x = insert (typeRep @a) x
buildBigMap n x = insert (typeRep @a) x
                . buildBigMap (n - 1) (Proxy @(a + 1))

newtype Hack f = Hack (TypeRepMap f)

instance NFData (Hack f) where
  rnf (Hack x) = 
    rnf . map (\(This t) -> typeRepFingerprint t) $ keys x

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
