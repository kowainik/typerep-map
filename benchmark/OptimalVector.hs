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
       ( spec
       ) where

import Spec
import Criterion.Main (bench, nf, env)

import Prelude hiding (lookup)

import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits

import Data.TypeRep.OptimalVector (TF (..), TypeRepMap (..), fromList, lookup)

spec :: BenchSpec
spec = BenchSpec
  { benchLookup = Just $ \name ->
      env mkBigMap $ \ ~bigMap ->
        bench name $ nf tenLookups bigMap
  , benchInsertSmall = Nothing -- Not implemented
  , benchInsertBig = Nothing -- Not implemented
  , benchUpdateSmall = Nothing -- Not implemented
  , benchUpdateBig = Nothing -- Not implemented
  }
  
tenLookups :: TypeRepMap (Proxy :: Nat -> *)
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a::Nat). Typeable a => Proxy a
    lp = fromJust $ lookup tmap

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

