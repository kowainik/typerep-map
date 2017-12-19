{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MagicHash, BangPatterns #-}


module Data.TypeRep.Vector
       ( TypeRepVector (..)
       , TF (..)
       , empty
       , insert
       , lookup
       , size
       , fromList
       ) where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base hiding (empty)
import GHC.Exts (sortWith)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.ST
import GHC.Prim

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Storable as Storable

data TypeRepVector f = TypeRepVect
    { fingerprints :: V.Vector Fingerprint
    , anys         :: V.Vector Any
    }

fromAny :: Any -> f a
fromAny = unsafeCoerce

-- | Empty structure.
empty :: TypeRepVector f
empty = TypeRepVect mempty mempty

-- | Inserts the value with its type as a key.
insert :: forall a f . Typeable a => a -> TypeRepVector f -> TypeRepVector f
insert = undefined

-- | Looks up the value at the type.
-- >>> let x = lookup $ insert (11 :: Int) empty
-- >>> x :: Maybe Int
-- Just 11
-- >>> x :: Maybe ()
-- Nothing
lookup :: forall a f . Typeable a => TypeRepVector f -> Maybe (f a)
lookup tVect =  fromAny . (anys tVect V.!)
            <$> binarySearch (typeRepFingerprint (typeRep (Proxy :: Proxy a))) (fingerprints tVect)

-- | Returns the size of the 'TypeRepVect'.
size :: TypeRepVector f -> Int
size = V.length . fingerprints

data TF f where
  TF :: Typeable a => f a -> TF f

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

fromList :: forall f . [TF f] -> TypeRepVector f
fromList tfs = TypeRepVect (V.fromList fps) (V.fromList ans)
  where
    (fps, ans) = unzip $ sortWith fst $ map (fp &&& an) tfs

    fp :: TF f -> Fingerprint
    fp (TF x) = typeRepFingerprint $ typeRep $ fromF x

    an :: TF f -> Any
    an (TF x) = unsafeCoerce x

-- | Returns the index is found.
binarySearch :: Fingerprint -> V.Vector Fingerprint -> Maybe Int
binarySearch fp fpVect =
    let
      !(I# len) = V.length fpVect
      ind = I# (binSearchHelp (-1#) len)
    in
      if fp == (fpVect V.! ind) then Just ind else Nothing
  where
    binSearchHelp :: Int# -> Int# -> Int#
    binSearchHelp l r = case l <# (r -# 1#) of
        0# -> r
        _ ->
            let m = uncheckedIShiftRA# (l +# r) 1# in
            if V.unsafeIndex fpVect (I# m) < fp
                then binSearchHelp m r
                else binSearchHelp l m
