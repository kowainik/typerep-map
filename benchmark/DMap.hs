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
       ( spec
       ) where

import Criterion.Main (bench, nf, whnf, env)

import Prelude hiding (lookup)

import Spec
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


spec :: BenchSpec
spec = BenchSpec
  { benchLookup = Just $ \name ->
      env mkBigMap $ \ ~(DMapNF bigMap) ->
        bench name $ nf tenLookups bigMap
  , benchInsertSmall = Just $ \name -> 
      bench name $ whnf (inserts empty 10) (Proxy @ 99999)
  , benchInsertBig = Just $ \name ->
      env mkBigMap $ \ ~(DMapNF bigMap) ->
       bench name $ whnf (inserts bigMap 1) (Proxy @ 99999)
  , benchUpdateSmall = Nothing -- Not implemented
  , benchUpdateBig = Nothing -- Not implemented
  }

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

-- | Wrapper that provides NFData instance to the 'DMap' structure.
newtype DMapNF f = DMapNF (TypeRepMap f)

instance NFData (DMapNF f) where
  rnf (DMapNF x) = 
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
