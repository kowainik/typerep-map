{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module CMap
       ( spec
       ) where

import Criterion.Main (bench, nf, env, whnf)

import Prelude hiding (lookup)

import Common
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits

import Data.TypeRep.CMap (TypeRepMap (..), empty, insert, lookup)

spec :: BenchSpec
spec = BenchSpec
  { benchLookup = Just $ \name ->
      env mkBigMap $ \ ~bigMap ->
        bench name $ nf tenLookups bigMap
  , benchInsertSmall = Just $ \name -> 
      bench name $ whnf (inserts empty 10) (Proxy :: Proxy 99999)
  , benchInsertBig = Just $ \name ->
      env mkBigMap $ \ ~(bigMap) ->
       bench name $ whnf (inserts bigMap 1) (Proxy :: Proxy 99999)
  , benchUpdateSmall = Just $ \name ->
      env mkSmallMap $ \ ~(smallMap) ->
      bench name $ whnf (updates smallMap 10) (Proxy :: Proxy 0)
  , benchUpdateBig = Just $ \name ->
      env mkBigMap $ \ ~(bigMap) ->
        bench name $ whnf (updates bigMap 10) (Proxy :: Proxy 0)
  }

tenLookups :: TypeRepMap (Proxy :: Nat -> *)
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a::Nat). Typeable a => Proxy a
    lp = fromJust $ lookup tmap

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

updates :: forall a . (KnownNat a)
        => TypeRepMap (Proxy :: Nat -> *)
        -> Int
        -> Proxy (a :: Nat)
        -> TypeRepMap (Proxy :: Nat -> *)
updates !c 0 _ = c
updates !c n x = inserts
   (insert x c)
   (n-1)
   (Proxy :: Proxy (a+1))

mkSmallMap :: IO (TypeRepMap (Proxy :: Nat -> *))
mkSmallMap = pure $ buildBigMap 10 (Proxy :: Proxy 0) empty

-- TypeRepMap of 10000 elements
mkBigMap :: IO (TypeRepMap (Proxy :: Nat -> *))
mkBigMap = pure $ buildBigMap 10000 (Proxy :: Proxy 0) empty

buildBigMap :: forall a . (KnownNat a) => Int -> Proxy (a :: Nat) -> TypeRepMap (Proxy :: Nat -> *) -> TypeRepMap (Proxy :: Nat -> *)
buildBigMap 1 x = insert x
buildBigMap n x = insert x . buildBigMap (n - 1) (Proxy :: Proxy (a + 1))
