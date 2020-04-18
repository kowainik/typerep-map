{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module DMap
    ( spec
    ) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData (..))
import Criterion.Main (bench, env, nf, whnf)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (type (+), KnownNat, Nat)
import Type.Reflection (TypeRep, Typeable, typeRep)
import Type.Reflection.Unsafe (typeRepFingerprint)

import Data.Dependent.Map (DMap, empty, insert, keys, lookup)
import Data.Some (Some (Some))

import Spec (BenchSpec (..))


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

tenLookups
    :: TypeRepMap (Proxy :: Nat -> Type)
    -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
       , Proxy 50, Proxy 60, Proxy 70, Proxy 80
       )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a :: Nat) . Typeable a => Proxy a
    lp = fromJust $ lookup (typeRep @a) tmap

inserts
    :: forall a . (KnownNat a)
    => TypeRepMap (Proxy :: Nat -> Type)
    -> Int
    -> Proxy (a :: Nat)
    -> TypeRepMap (Proxy :: Nat -> Type)
inserts !c 0 _ = c
inserts !c n x = inserts
    (insert (typeRep @ a) x c)
    (n-1)
    (Proxy :: Proxy (a+1))

-- TypeRepMap of 10000 elements
mkBigMap :: IO (DMapNF (Proxy :: Nat -> Type))
mkBigMap = pure . DMapNF $ buildBigMap 10000 (Proxy :: Proxy 0) empty

buildBigMap
    :: forall a . (KnownNat a)
    => Int
    -> Proxy (a :: Nat)
    -> TypeRepMap (Proxy :: Nat -> Type)
    -> TypeRepMap (Proxy :: Nat -> Type)
buildBigMap 1 x = insert (typeRep @a) x
buildBigMap n x = insert (typeRep @a) x
                . buildBigMap (n - 1) (Proxy @(a + 1))

-- | Wrapper that provides NFData instance to the 'DMap' structure.
newtype DMapNF f = DMapNF (TypeRepMap f)

instance NFData (DMapNF f) where
    rnf :: DMapNF f -> ()
    rnf (DMapNF x) =
        rnf . map (\(Some t) -> typeRepFingerprint t) $ keys x
