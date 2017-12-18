{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module Data.TypeRep.Vector
       ( TypeRepVector (..)
       , empty
       , insert
       , lookup
       , size
       ) where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base (Any)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as Unboxed

data instance Unboxed.MVector s Fingerprint = MFingerprintVector !Int (Unboxed.MVector s Word64) (Unboxed.MVector s Word64)
data instance Unboxed.Vector Fingerprint = FingerprintVector !Int (Unboxed.Vector Word64) (Unboxed.Vector Word64)

instance Unboxed.Unbox Fingerprint

instance M.MVector Unboxed.MVector Fingerprint where
    {-# INLINE basicLength  #-}
    basicLength (MFingerprintVector n _ _) = n
    {-# INLINE basicUnsafeSlice  #-}
    basicUnsafeSlice i m (MFingerprintVector _ a b) =
        MFingerprintVector m (M.basicUnsafeSlice i m a) (M.basicUnsafeSlice i m b)
    {-# INLINE basicOverlaps  #-}
    basicOverlaps (MFingerprintVector _ as1 bs1) (MFingerprintVector _ as2 bs2) =
        M.basicOverlaps as1 as2 || M.basicOverlaps bs1 bs2
    {-# INLINE basicUnsafeNew  #-}
    basicUnsafeNew n_
        = do
            as <- M.basicUnsafeNew n_
            bs <- M.basicUnsafeNew n_
            return $ MFingerprintVector n_ as bs
    {-# INLINE basicInitialize  #-}
    basicInitialize (MFingerprintVector _ as bs)
        = do
            M.basicInitialize as
            M.basicInitialize bs
    {-# INLINE basicUnsafeReplicate  #-}
    basicUnsafeReplicate n_ (Fingerprint a b)
        = do
            as <- M.basicUnsafeReplicate n_ a
            bs <- M.basicUnsafeReplicate n_ b
            return $ MFingerprintVector n_ as bs
    {-# INLINE basicUnsafeRead  #-}
    basicUnsafeRead (MFingerprintVector _ as bs) i_
        = do
            a <- M.basicUnsafeRead as i_
            b <- M.basicUnsafeRead bs i_
            return (Fingerprint a b)
    {-# INLINE basicUnsafeWrite  #-}
    basicUnsafeWrite (MFingerprintVector _ as bs) i_ (Fingerprint a b)
        = do
            M.basicUnsafeWrite as i_ a
            M.basicUnsafeWrite bs i_ b
    {-# INLINE basicClear  #-}
    basicClear (MFingerprintVector _ as bs)
        = do
            M.basicClear as
            M.basicClear bs
    {-# INLINE basicSet  #-}
    basicSet (MFingerprintVector _ as bs) (Fingerprint a b)
        = do
            M.basicSet as a
            M.basicSet bs b
    {-# INLINE basicUnsafeCopy  #-}
    basicUnsafeCopy (MFingerprintVector _ as1 bs1) (MFingerprintVector _ as2 bs2)
        = do
            M.basicUnsafeCopy as1 as2
            M.basicUnsafeCopy bs1 bs2
    {-# INLINE basicUnsafeMove  #-}
    basicUnsafeMove (MFingerprintVector _ as1 bs1) (MFingerprintVector _ as2 bs2)
        = do
            M.basicUnsafeMove as1 as2
            M.basicUnsafeMove bs1 bs2
    {-# INLINE basicUnsafeGrow  #-}
    basicUnsafeGrow (MFingerprintVector n_ as bs) m_
        = do
            as' <- M.basicUnsafeGrow as m_
            bs' <- M.basicUnsafeGrow bs m_
            return $ MFingerprintVector (m_ + n_) as' bs'

instance G.Vector Unboxed.Vector Fingerprint where
    {-# INLINE basicUnsafeFreeze  #-}
    basicUnsafeFreeze (MFingerprintVector n_ as bs)
        = do
            as' <- G.basicUnsafeFreeze as
            bs' <- G.basicUnsafeFreeze bs
            return $ FingerprintVector n_ as' bs'
    {-# INLINE basicUnsafeThaw  #-}
    basicUnsafeThaw (FingerprintVector n_ as bs)
        = do
            as' <- G.basicUnsafeThaw as
            bs' <- G.basicUnsafeThaw bs
            return $ MFingerprintVector n_ as' bs'
    {-# INLINE basicLength  #-}
    basicLength (FingerprintVector n_ _ _) = n_
    {-# INLINE basicUnsafeSlice  #-}
    basicUnsafeSlice i_ m_ (FingerprintVector _ as bs)
        = FingerprintVector m_ (G.basicUnsafeSlice i_ m_ as)
                 (G.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicUnsafeIndexM  #-}
    basicUnsafeIndexM (FingerprintVector _ as bs) i_
        = do
            a <- G.basicUnsafeIndexM as i_
            b <- G.basicUnsafeIndexM bs i_
            return (Fingerprint a b)
    {-# INLINE basicUnsafeCopy  #-}
    basicUnsafeCopy (MFingerprintVector _ as1 bs1) (FingerprintVector _ as2 bs2)
        = do
            G.basicUnsafeCopy as1 as2
            G.basicUnsafeCopy bs1 bs2
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
