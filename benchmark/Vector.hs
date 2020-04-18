{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Vector
    ( benchVector
    , prepareBenchVector
    ) where

import Prelude hiding (lookup)

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits (type (+), KnownNat, Nat)

import Data.TypeRep.Vector (TF (..), TypeRepVector, fingerprints, fromList, lookup)


benchVector :: Benchmark
benchVector = bgroup "vector"
    [ bench "lookup"     $ nf tenLookups bigMap
    -- , bench "insert new" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 9999999999)
    -- , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) (Proxy :: Proxy 1)
    ]

tenLookups
    :: TypeRepVector (Proxy :: Nat -> Type)
    -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
       , Proxy 50, Proxy 60, Proxy 70, Proxy 80
       )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a::Nat). Typeable a => Proxy a
    lp = fromJust $ lookup tmap

-- TypeRepMap of 10000 elements
bigMap :: TypeRepVector (Proxy :: Nat -> Type)
bigMap = fromList $ buildBigMap 10000 (Proxy :: Proxy 0) []

buildBigMap
    :: forall a . (KnownNat a)
    => Int
    -> Proxy (a :: Nat)
    -> [TF (Proxy :: Nat -> Type)]
    -> [TF (Proxy :: Nat -> Type)]
buildBigMap 1 x = (TF x :)
buildBigMap n x = (TF x :) . buildBigMap (n - 1) (Proxy :: Proxy (a + 1))

rknf :: TypeRepVector f -> ()
rknf = rnf . fingerprints

prepareBenchVector :: IO ()
prepareBenchVector = evaluate (rknf bigMap)
