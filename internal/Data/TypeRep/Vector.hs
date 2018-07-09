{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

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

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as Unboxed

data instance Unboxed.MVector s Fingerprint = MFingerprintVector (Unboxed.MVector s Word64) (Unboxed.MVector s Word64)
data instance Unboxed.Vector Fingerprint = FingerprintVector (Unboxed.Vector Word64) (Unboxed.Vector Word64)

instance Unboxed.Unbox Fingerprint

instance M.MVector Unboxed.MVector Fingerprint where
    {-# INLINE basicLength  #-}
    basicLength (MFingerprintVector x _) = M.basicLength x
    {-# INLINE basicUnsafeSlice  #-}
    basicUnsafeSlice i m (MFingerprintVector a b) =
        MFingerprintVector (M.basicUnsafeSlice i m a) (M.basicUnsafeSlice i m b)
    {-# INLINE basicOverlaps  #-}
    basicOverlaps (MFingerprintVector as1 bs1) (MFingerprintVector as2 bs2) =
        M.basicOverlaps as1 as2 || M.basicOverlaps bs1 bs2
    {-# INLINE basicUnsafeNew  #-}
    basicUnsafeNew n_ = do
        as <- M.basicUnsafeNew n_
        bs <- M.basicUnsafeNew n_
        return $ MFingerprintVector as bs
    {-# INLINE basicInitialize  #-}
    basicInitialize (MFingerprintVector as bs) = do
        M.basicInitialize as
        M.basicInitialize bs
    {-# INLINE basicUnsafeReplicate  #-}
    basicUnsafeReplicate n_ (Fingerprint a b) = do
        as <- M.basicUnsafeReplicate n_ a
        bs <- M.basicUnsafeReplicate n_ b
        return $ MFingerprintVector as bs
    {-# INLINE basicUnsafeRead  #-}
    basicUnsafeRead (MFingerprintVector as bs) i_ = do
        a <- M.basicUnsafeRead as i_
        b <- M.basicUnsafeRead bs i_
        return (Fingerprint a b)
    {-# INLINE basicUnsafeWrite  #-}
    basicUnsafeWrite (MFingerprintVector as bs) i_ (Fingerprint a b) = do
        M.basicUnsafeWrite as i_ a
        M.basicUnsafeWrite bs i_ b
    {-# INLINE basicClear  #-}
    basicClear (MFingerprintVector as bs) = do
        M.basicClear as
        M.basicClear bs
    {-# INLINE basicSet  #-}
    basicSet (MFingerprintVector as bs) (Fingerprint a b) = do
        M.basicSet as a
        M.basicSet bs b
    {-# INLINE basicUnsafeCopy  #-}
    basicUnsafeCopy (MFingerprintVector as1 bs1) (MFingerprintVector as2 bs2) = do
        M.basicUnsafeCopy as1 as2
        M.basicUnsafeCopy bs1 bs2
    {-# INLINE basicUnsafeMove  #-}
    basicUnsafeMove (MFingerprintVector as1 bs1) (MFingerprintVector as2 bs2) = do
        M.basicUnsafeMove as1 as2
        M.basicUnsafeMove bs1 bs2
    {-# INLINE basicUnsafeGrow  #-}
    basicUnsafeGrow (MFingerprintVector as bs) m_ = do
        as' <- M.basicUnsafeGrow as m_
        bs' <- M.basicUnsafeGrow bs m_
        return $ MFingerprintVector as' bs'

instance G.Vector Unboxed.Vector Fingerprint where
    {-# INLINE basicUnsafeFreeze  #-}
    basicUnsafeFreeze (MFingerprintVector as bs) = do
        as' <- G.basicUnsafeFreeze as
        bs' <- G.basicUnsafeFreeze bs
        return $ FingerprintVector as' bs'
    {-# INLINE basicUnsafeThaw  #-}
    basicUnsafeThaw (FingerprintVector as bs) = do
        as' <- G.basicUnsafeThaw as
        bs' <- G.basicUnsafeThaw bs
        return $ MFingerprintVector as' bs'
    {-# INLINE basicLength  #-}
    basicLength (FingerprintVector x _) = G.basicLength x
    {-# INLINE basicUnsafeSlice  #-}
    basicUnsafeSlice i_ m_ (FingerprintVector as bs) =
        FingerprintVector (G.basicUnsafeSlice i_ m_ as) (G.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicUnsafeIndexM  #-}
    basicUnsafeIndexM (FingerprintVector as bs) i_ = do
        a <- G.basicUnsafeIndexM as i_
        b <- G.basicUnsafeIndexM bs i_
        return (Fingerprint a b)
    {-# INLINE basicUnsafeCopy  #-}
    basicUnsafeCopy (MFingerprintVector as1 bs1) (FingerprintVector as2 bs2) = do
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
    let
      !(I# len) = Unboxed.length fpVect
      ind = I# (binSearchHelp (-1#) len)
    in
      if fp == (fpVect Unboxed.! ind) then Just ind else Nothing
  where
    binSearchHelp :: Int# -> Int# -> Int#
    binSearchHelp l r = case l <# (r -# 1#) of
        0# -> r
        _ ->
            let m = uncheckedIShiftRA# (l +# r) 1# in
            if Unboxed.unsafeIndex fpVect (I# m) < fp
                then binSearchHelp m r
                else binSearchHelp l m
