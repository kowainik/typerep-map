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
       , prepareBenchVectorOpt
       ) where

import Criterion.Main (Benchmark, bench, bgroup, nf)

import Prelude hiding (lookup)

import Control.Exception
import Control.DeepSeq (rnf)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits

import Data.TypeRep.OptimalVector

benchVectorOpt :: Benchmark
benchVectorOpt = bgroup "vector optimal"
   [ bench "lookup"     $ nf tenLookups bigMap
   -- , bench "insert new" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 9999999999)
   -- , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 1)
   ]

tenLookups :: TypeRepVector (Proxy :: Nat -> *)
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a::Nat). Typeable a => Proxy a
    lp = fromJust $ lookup tmap

-- TypeRepMap of 10000 elements
bigMap :: TypeRepVector (Proxy :: Nat -> *)
bigMap = fromList $ buildBigMap 10000 (Proxy :: Proxy 0) []

buildBigMap :: forall a . (Typeable a, KnownNat a) => Int -> Proxy (a :: Nat) -> [TF (Proxy :: Nat -> *)] -> [TF (Proxy :: Nat -> *)]
buildBigMap 1 x = (TF x :)
buildBigMap n x = (TF x :) . buildBigMap (n - 1) (Proxy :: Proxy (a + 1))

rknf :: TypeRepVector f -> ()
rknf = rnf . fingerprints

prepareBenchVectorOpt :: IO ()
prepareBenchVectorOpt = evaluate (rknf bigMap)
