{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Data.TypeRep.Vector
       ( TypeRepVector (..)
       , empty
       , insert
       , lookup
       , size
       ) where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base (Any)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Unboxed

type FingerprintPair = (Word64, Word64)

data TypeRepVector f = TypeRepVect
    { fingerprints :: Unboxed.Vector FingerprintPair
    , anys         :: V.Vector Any
    }

fromFingerprint :: Fingerprint -> FingerprintPair
fromFingerprint (Fingerprint x y) = (x, y)

typeRepToPair :: TypeRep -> FingerprintPair
typeRepToPair = fromFingerprint . typeRepFingerprint

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
lookup tVect = case binarySearch (typeRepToPair (typeRep (Proxy :: Proxy a))) (fingerprints tVect) of
     Just ind -> Just $ unsafeCoerce $ (anys tVect) V.! ind
     Nothing  -> Nothing

-- | Returns the size of the 'TypeRepVect'.
size :: TypeRepVector f -> Int
size = Unboxed.length . fingerprints

-- | Returns the index is found.
binarySearch :: FingerprintPair -> Unboxed.Vector FingerprintPair -> Maybe Int
binarySearch fp fpVect =
    let ind = binSearchHelp (-1) (Unboxed.length fpVect) in
    if fp == (fpVect Unboxed.! ind) then Just ind else Nothing
  where
    binSearchHelp :: Int -> Int -> Int
    binSearchHelp l r = if l < r - 1
        then (
            let m = (l + r) `div` 2 in
            if fpVect Unboxed.! m < fp
                then binSearchHelp m r
                else binSearchHelp l m )
        else r
