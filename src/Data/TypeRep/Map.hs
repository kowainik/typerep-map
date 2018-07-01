{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.TypeRep.Map
       ( -- * Map type
         TypeRepMap (..)

         -- 'TypeRepMap' interface
       , empty
       , insert
       , lookup
       , size

         -- * Helpful testing functions
       , TF (..)
       , fromList
       ) where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base (Any, Int (..), Int#, uncheckedIShiftRA#, (+#), (-#), (<#))
import GHC.Exts (inline, sortWith)
import GHC.Fingerprint (Fingerprint (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Unboxed

data TypeRepMap (f :: k -> Type) = TypeRepMap
    { fingerprintAs :: Unboxed.Vector Word64
    , fingerprintBs :: Unboxed.Vector Word64
    , anys          :: V.Vector Any
    }

fromAny :: Any -> f a
fromAny = unsafeCoerce

-- | Empty structure.
empty :: TypeRepMap f
empty = TypeRepMap mempty mempty mempty

-- | Inserts the value with its type as a key.
insert :: forall a f . Typeable a => a -> TypeRepMap f -> TypeRepMap f
insert = undefined

-- | Looks up the value at the type.
-- >>> let x = lookup $ insert (11 :: Int) empty
-- >>> x :: Maybe Int
-- Just 11
-- >>> x :: Maybe ()
-- Nothing
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
lookup tVect =  fromAny . (anys tVect V.!)
            <$> binarySearch (typeRepFingerprint $ typeRep $ Proxy @a)
                             (fingerprintAs tVect)
                             (fingerprintBs tVect)

-- | Returns the size of the 'TypeRepMap'.
size :: TypeRepMap f -> Int
size = Unboxed.length . fingerprintAs

-- | Returns the index is found.
binarySearch :: Fingerprint -> Unboxed.Vector Word64 -> Unboxed.Vector Word64 -> Maybe Int
binarySearch (Fingerprint a b) fpAs fpBs =
    let
      !(I# len) = Unboxed.length fpAs
      checkfpBs :: Int# -> Maybe Int
      checkfpBs i =
        case i <# len of
          0# -> Nothing
          _ ->
            if a /= Unboxed.unsafeIndex fpAs (I# i)
            then Nothing
            else if b == Unboxed.unsafeIndex fpBs (I# i)
                 then Just (I# i)
                 else checkfpBs (i +# 1#)
    in
      inline (checkfpBs (binSearchHelp (-1#) len))
  where
    binSearchHelp :: Int# -> Int# -> Int#
    binSearchHelp l r = case l <# (r -# 1#) of
        0# -> r
        _  ->
            let m = uncheckedIShiftRA# (l +# r) 1# in
            if Unboxed.unsafeIndex fpAs (I# m) < a
                then binSearchHelp m r
                else binSearchHelp l m

----------------------------------------------------------------------------
-- Functions for testing and benchmarking
----------------------------------------------------------------------------

data TF f where
  TF :: Typeable a => f a -> TF f

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

fromList :: forall f . [TF f] -> TypeRepMap f
fromList tfs = TypeRepMap (Unboxed.fromList fpAs) (Unboxed.fromList fpBs) (V.fromList ans)
  where
    (fpAs, fpBs) = unzip $ fmap (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans) = unzip $ sortWith fst $ map (fp &&& an) tfs

    fp :: TF f -> Fingerprint
    fp (TF x) = typeRepFingerprint $ typeRep $ fromF x

    an :: TF f -> Any
    an (TF x) = unsafeCoerce x
