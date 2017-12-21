{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Vector
       ( benchVector
       , prepareBenchVector
       ) where

import Criterion.Main (Benchmark, bench, bgroup, nf)

import Prelude hiding (lookup)

import Control.Exception
import Control.DeepSeq (rnf)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits

import Data.TypeRep.Vector

benchVector :: Benchmark
benchVector = bgroup "vector"
    [ bench "lookup"     $ nf tenLookups bigMap
    -- , bench "insert new" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 9999999999)
    -- , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 1)
    ]

tenLookups :: TypeRepVector (Proxy :: Nat -> *)
           -> ( Proxy 1000, Proxy 2000, Proxy 3000, Proxy 4000
              , Proxy 5000, Proxy 6000, Proxy 7000, Proxy 8000
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

prepareBenchVector :: IO ()
prepareBenchVector = evaluate (rknf bigMap)
