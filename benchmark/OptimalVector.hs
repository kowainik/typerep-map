{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module OptimalVector
       ( benchVectorOpt
       ) where

import Criterion.Main (Benchmark, bench, bgroup, nf, env)

import Prelude hiding (lookup)

import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits

import Data.TypeRep.OptimalVector (TF (..), TypeRepMap (..), fromList, lookup)

benchVectorOpt :: Benchmark
benchVectorOpt =
  env mkBigMap $ \ ~bigMap ->
   bgroup "vector optimal"
     [ bench "lookup"     $ nf tenLookups bigMap
     -- , bench "insert new 10 elements" $ whnf (inserts empty 10) (Proxy :: Proxy 0)
     -- , bench "insert big 1 element" $ whnf (inserts bigMap 1) (Proxy :: Proxy 0)
     -- , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 1)
     ]
  
tenLookups :: TypeRepMap (Proxy :: Nat -> *)
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a::Nat). Typeable a => Proxy a
    lp = fromJust $ lookup tmap

{-
 - XXX: not yet implemented
inserts :: forall a . (KnownNat a)
        => TypeRepMap (Proxy :: Nat -> *)
        -> Int
        -> Proxy (a :: Nat)
        -> TypeRepMap (Proxy :: Nat -> *)
inserts !c 0 _ = c
inserts !c n x = inserts
   (insert x c)
   (n-1)
   (Proxy :: Proxy (a+1))
-}

-- TypeRepMap of 10000 elements
mkBigMap :: IO (TypeRepMap (Proxy :: Nat -> *))
mkBigMap = pure $ fromList $ buildBigMap 10000 (Proxy :: Proxy 0) []

buildBigMap :: forall a . (KnownNat a)
            => Int
            -> Proxy (a :: Nat)
            -> [TF (Proxy :: Nat -> *)]
            -> [TF (Proxy :: Nat -> *)]
buildBigMap 1 x = (TF x :)
buildBigMap n x = (TF x :) . buildBigMap (n - 1) (Proxy :: Proxy (a + 1))

