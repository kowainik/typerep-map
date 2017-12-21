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
{-# LANGUAGE RecordWildCards       #-}


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
import GHC.Base hiding (empty)
import GHC.Exts (sortWith)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Data.Primitive.ByteArray
import GHC.Word

data TypeRepVector (f :: k -> *) = TypeRepVect
    { fingerprintAs :: ByteArray#
    , fingerprintBs :: ByteArray#
    , anys          :: V.Vector Any
    }

-- | Empty structure.
empty :: TypeRepVector f
empty = fromList []

-- | Inserts the value with its type as a key.
insert :: forall a f . Typeable a => a -> TypeRepVector f -> TypeRepVector f
insert _ = id

-- | Looks up the value at the type.
-- >>> let x = lookup $ insert (11 :: Int) empty
-- >>> x :: Maybe Int
-- Just 11
-- >>> x :: Maybe ()
-- Nothing
lookup :: forall a f . Typeable a => TypeRepVector f -> Maybe (f a)
lookup TypeRepVect{..} =
  fromAny (binarySearch anys fp fingerprintAs fingerprintBs)
  where
    fp = typeRepFingerprint (typeRep (Proxy :: Proxy a))
    fromAny = unsafeCoerce :: Maybe Any -> Maybe (f a)

-- | Returns the size of the 'TypeRepVect'.
size :: TypeRepVector f -> Int
size = V.length . anys

data TF f where
  TF :: Typeable a => f a -> TF f

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

fromList :: forall f . [TF f] -> TypeRepVector f
fromList tfs = TypeRepVect fpAs' fpBs' (V.fromList ans)
  where
    !(P.Vector _ _ (ByteArray fpAs')) = P.fromList fpAs
    !(P.Vector _ _ (ByteArray fpBs')) = P.fromList fpBs
    (fpAs, fpBs) = unzip $ fmap (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans) = unzip $ sortWith fst $ map (fp &&& an) tfs

    fp :: TF f -> Fingerprint
    fp (TF x) = typeRepFingerprint $ typeRep $ fromF x

    an :: TF f -> Any
    an (TF x) = unsafeCoerce x

-- | Returns the index is found.
binarySearch :: forall a. V.Vector a -> Fingerprint -> ByteArray# -> ByteArray# -> Maybe a
binarySearch !anys !(Fingerprint (W64# a) (W64# b)) !fpAs !fpBs =
    let
      !(I# len) = V.length anys
      checkfpBs :: Int# -> Maybe a
      checkfpBs !i =
        case i <# len of
          0# -> Nothing
          _ ->
            case eqWord# a (indexWord64Array# fpAs i) of
              0# -> Nothing
              _ ->
                 case eqWord# b (indexWord64Array# fpBs i) of
                   0# -> checkfpBs (i +# 1#)
                   _ -> Just (V.unsafeIndex anys (I# i))
    in
      inline (checkfpBs (binSearchHelp (-1#) len))
  where
    binSearchHelp :: Int# -> Int# -> Int#
    binSearchHelp !l !r = case l <# (r -# 1#) of
        0# -> r
        _  ->
            let m = uncheckedIShiftRA# (l +# r) 1# in
            case ltWord# (indexWord64Array# fpAs m) a of
              0# -> binSearchHelp l m
              _  -> binSearchHelp m r

