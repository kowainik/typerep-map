{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module Data.TypeRep.OptimalVector
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
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base (Any, liftM)
import GHC.Exts (sortWith)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as Unboxed

data instance Unboxed.MVector s Fingerprint = MFingerprintVectorOpt (Unboxed.MVector s Word64)
data instance Unboxed.Vector Fingerprint = FingerprintVectorOpt (Unboxed.Vector Word64)

instance Unboxed.Unbox Fingerprint

instance M.MVector Unboxed.MVector Fingerprint where
    {-# INLINE basicLength  #-}
    basicLength (MFingerprintVectorOpt v) = M.basicLength v `div` 2
    {-# INLINE basicUnsafeSlice  #-}
    basicUnsafeSlice i n (MFingerprintVectorOpt v) =
        MFingerprintVectorOpt $ M.basicUnsafeSlice (2 * i) (2 * n) v
    {-# INLINE basicOverlaps  #-}
    basicOverlaps (MFingerprintVectorOpt v1) (MFingerprintVectorOpt v2) =
        M.basicOverlaps v1 v2
    {-# INLINE basicUnsafeNew  #-}
    basicUnsafeNew n = MFingerprintVectorOpt `liftM` M.basicUnsafeNew (2 * n)
    {-# INLINE basicInitialize  #-}
    basicInitialize (MFingerprintVectorOpt v) = M.basicInitialize v
    {-# INLINE basicUnsafeReplicate  #-}
    basicUnsafeReplicate n_ (Fingerprint a b) = do
        v <- M.basicUnsafeNew $ 2 * n_
        for_ [0, 2 .. 2 * n_ - 1] $ \i -> do
            M.basicUnsafeWrite v i a
            M.basicUnsafeWrite v (i + 1) b
        return $ MFingerprintVectorOpt v
    {-# INLINE basicUnsafeRead  #-}
    basicUnsafeRead (MFingerprintVectorOpt v) i = do
        a <- M.basicUnsafeRead v (2 * i)
        b <- M.basicUnsafeRead v (2 * i + 1)
        return (Fingerprint a b)
    {-# INLINE basicUnsafeWrite  #-}
    basicUnsafeWrite (MFingerprintVectorOpt v) i (Fingerprint a b) = do
        M.basicUnsafeWrite v (2 * i) a
        M.basicUnsafeWrite v (2 * i + 1) b
    {-# INLINE basicClear  #-}
    basicClear (MFingerprintVectorOpt v) = M.basicClear v
    {-# INLINE basicSet  #-}
    basicSet (MFingerprintVectorOpt v) (Fingerprint a b) = do
        let n = M.basicLength v
        for_ [0, 2 .. n - 1] $ \i -> do
            M.basicUnsafeWrite v i a
            M.basicUnsafeWrite v (i + 1) b
    {-# INLINE basicUnsafeCopy  #-}
    basicUnsafeCopy (MFingerprintVectorOpt v1) (MFingerprintVectorOpt v2) =
        M.basicUnsafeCopy v1 v2
    {-# INLINE basicUnsafeMove  #-}
    basicUnsafeMove (MFingerprintVectorOpt v1) (MFingerprintVectorOpt v2) =
        M.basicUnsafeMove v1 v2
    {-# INLINE basicUnsafeGrow  #-}
    basicUnsafeGrow (MFingerprintVectorOpt v) m_ =
        MFingerprintVectorOpt <$> M.basicUnsafeGrow v (2 * m_)

instance G.Vector Unboxed.Vector Fingerprint where
    {-# INLINE basicUnsafeFreeze  #-}
    basicUnsafeFreeze (MFingerprintVectorOpt v) =
        FingerprintVectorOpt <$> G.basicUnsafeFreeze v
    {-# INLINE basicUnsafeThaw  #-}
    basicUnsafeThaw (FingerprintVectorOpt v) = do
        MFingerprintVectorOpt <$> G.basicUnsafeThaw v
    {-# INLINE basicLength  #-}
    basicLength (FingerprintVectorOpt v) = G.basicLength v `div` 2
    {-# INLINE basicUnsafeSlice  #-}
    basicUnsafeSlice i_ m_ (FingerprintVectorOpt v) =
        FingerprintVectorOpt (G.basicUnsafeSlice (2 * i_) (2 * m_) v)
    {-# INLINE basicUnsafeIndexM  #-}
    basicUnsafeIndexM (FingerprintVectorOpt v) i_ = do
            a <- G.basicUnsafeIndexM v (2 * i_)
            b <- G.basicUnsafeIndexM v (2 * i_ + 1)
            return (Fingerprint a b)
    {-# INLINE basicUnsafeCopy  #-}
    basicUnsafeCopy (MFingerprintVectorOpt v1) (FingerprintVectorOpt v2) =
        G.basicUnsafeCopy v1 v2
    {-# INLINE elemseq  #-}
    elemseq _ (Fingerprint a b)
        = G.elemseq (undefined :: Unboxed.Vector a) a
        . G.elemseq (undefined :: Unboxed.Vector b) b

data TypeRepVector f = TypeRepVect
    { fingerprints :: Unboxed.Vector Fingerprint
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
size = Unboxed.length . fingerprints

data TF f where
  TF :: Typeable a => f a -> TF f

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

fromList :: forall f . [TF f] -> TypeRepVector f
fromList tfs = TypeRepVect (Unboxed.fromList fps) (V.fromList ans)
  where
    (fps, ans) = unzip $ sortWith fst $ map (fp &&& an) tfs

    fp :: TF f -> Fingerprint
    fp (TF x) = typeRepFingerprint $ typeRep $ fromF x

    an :: TF f -> Any
    an (TF x) = unsafeCoerce x


-- | Returns the index is found.
binarySearch :: Fingerprint -> Unboxed.Vector Fingerprint -> Maybe Int
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
