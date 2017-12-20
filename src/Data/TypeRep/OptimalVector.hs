{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
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
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base hiding (empty)
import GHC.Exts (sortWith)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Unboxed

data TypeRepVector f = TypeRepVect
    { fingerprintAs :: Unboxed.Vector Word64
    , fingerprintBs :: Unboxed.Vector Word64
    , anys          :: V.Vector Any
    }

fromAny :: Any -> f a
fromAny = unsafeCoerce

-- | Empty structure.
empty :: TypeRepVector f
empty = TypeRepVect mempty mempty mempty

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
            <$> binarySearch (typeRepFingerprint (typeRep (Proxy :: Proxy a))) (fingerprintAs tVect) (fingerprintBs tVect)

-- | Returns the size of the 'TypeRepVect'.
size :: TypeRepVector f -> Int
size = Unboxed.length . fingerprintAs

data TF f where
  TF :: Typeable a => f a -> TF f

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

fromList :: forall f . [TF f] -> TypeRepVector f
fromList tfs = TypeRepVect (Unboxed.fromList fpAs) (Unboxed.fromList fpBs) (V.fromList ans)
  where
    (fpAs, fpBs) = unzip $ fmap (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans) = unzip $ sortWith fst $ map (fp &&& an) tfs

    fp :: TF f -> Fingerprint
    fp (TF x) = typeRepFingerprint $ typeRep $ fromF x

    an :: TF f -> Any
    an (TF x) = unsafeCoerce x

-- | Returns the index is found.
binarySearch :: Fingerprint -> Unboxed.Vector Word64 -> Unboxed.Vector Word64 -> Maybe Int
binarySearch (Fingerprint a b) fpAs fpBs =
    let
      !(I# len) = Unboxed.length fpAs
      ind = I# (binSearchHelp (-1#) len)
    in
      checkfpBs ind (I# len)
  where
    binSearchHelp :: Int# -> Int# -> Int#
    binSearchHelp l r = case l <# (r -# 1#) of
        0# -> r
        _  ->
            let m = uncheckedIShiftRA# (l +# r) 1# in
            if Unboxed.unsafeIndex fpAs (I# m) < a
                then binSearchHelp m r
                else binSearchHelp l m

    checkfpBs :: Int -> Int -> Maybe Int
    checkfpBs i len
        | i >= len || a /= Unboxed.unsafeIndex fpAs i = Nothing
        | b == Unboxed.unsafeIndex fpBs i = Just i
        | otherwise = checkfpBs (i + 1) len
