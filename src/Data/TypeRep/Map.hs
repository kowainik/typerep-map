{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes #-}

-- | This module introduces the 'TypeRepMap' data structure and functions to work with it.

module Data.TypeRep.Map
       ( -- * Map type
         TypeRepMap (..)

         -- * Construction
       , empty
       , one

         -- * Modification
       , insert
       , delete
       , hoist
       , unionWith
       , union

         -- * Query
       , lookup
       , member
       , size

         -- * Helpful testing functions
       , TF (..)
       , fromList
       , toFingerprints
       ) where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.List (nubBy)
import Data.Maybe (fromJust)
import Data.Primitive.Array (Array, indexArray, mapArray')
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import GHC.Base (Any, Int (..), Int#, (*#), (+#), (<#))
import GHC.Exts (inline, sortWith)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Prim (eqWord#, ltWord#)
import GHC.Word (Word64 (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified GHC.Exts as GHC (fromList, toList)

-- | Map-like data structure that keeps types as keys.
data TypeRepMap (f :: k -> Type) = TypeRepMap
    { fingerprintAs :: {-# UNPACK #-} !(PrimArray Word64)
    , fingerprintBs :: {-# UNPACK #-} !(PrimArray Word64)
    , anys          :: {-# UNPACK #-} !(Array Any)
    }

-- | Shows only 'Fingerprint's.
instance Show (TypeRepMap f) where
    show = show . toFingerprints

-- | Uses 'union' to combine 'TypeRepMap's.
instance Semigroup (TypeRepMap f) where
    (<>) :: TypeRepMap f -> TypeRepMap f -> TypeRepMap f
    (<>) = union
    {-# INLINE (<>) #-}

instance Monoid (TypeRepMap f) where
    mempty = TypeRepMap mempty mempty mempty
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

-- | Returns the list of 'Fingerprint's from 'TypeRepMap'.
toFingerprints :: TypeRepMap f -> [Fingerprint]
toFingerprints TypeRepMap{..} =
    zipWith Fingerprint (GHC.toList fingerprintAs) (GHC.toList fingerprintBs)

-- | Empty structure.
empty :: TypeRepMap f
empty = mempty
{-# INLINE empty #-}

-- | Construct a 'TypeRepMap' with a single element.
one :: forall a f . Typeable a => f a -> TypeRepMap f
one x = insert x empty
{-# INLINE one #-}

-- | Inserts the value with its type as a key.
insert :: forall a f . Typeable a => f a -> TypeRepMap f -> TypeRepMap f
insert x = fromListPairs . addX . toPairList
  where
    pairX :: (Fingerprint, Any)
    pairX@(fpX, _) = (calcFp x, toAny x)

    addX :: [(Fingerprint, Any)] -> [(Fingerprint, Any)]
    addX l = pairX : deleteByFst fpX l
{-# INLINE insert #-}

-- Helper type to workaround some issues with types
type KindOf (a :: k) = k

{- | Deletes value from list.

>>> let trMap = delete @Bool $ insert (Identity True) $ one (Identity 'a')
>>> size trMap
1
>>> member @Bool trMap
False
>>> member @Char trMap
True
-}
delete :: forall a (f :: KindOf a -> Type) . Typeable a => TypeRepMap f -> TypeRepMap f
delete = fromListPairs . deleteByFst (typeFp @a) . toPairList
{-# INLINE delete #-}

{- | Map over elements.

>>> let trMap = insert (Identity True) $ one (Identity 'a')
>>> lookup @Bool trMap
Just (Identity True)
>>> lookup @Char trMap
Just (Identity 'a')
>>> let trMap2 = hoist ((:[]) . runIdentity) trMap
>>> lookup @Bool trMap2
Just [True]
>>> lookup @Char trMap2
Just "a"
-}
hoist :: (forall x. f x -> g x) -> TypeRepMap f -> TypeRepMap g
hoist f (TypeRepMap as bs ans) = TypeRepMap as bs $ mapArray' (toAny . f . fromAny) ans
{-# INLINE hoist #-}

-- | The union with a combining function.
unionWith :: (forall x. f x -> f x -> f x) -> TypeRepMap f -> TypeRepMap f -> TypeRepMap f
unionWith f m1 m2 = fromListPairs
                  $ Map.toList
                  $ Map.unionWith combine
                                  (Map.fromList $ toPairList m1)
                                  (Map.fromList $ toPairList m2)
  where
    combine :: Any -> Any -> Any
    combine a b = toAny $ f (fromAny a) (fromAny b)
{-# INLINE unionWith #-}

-- | The (left-biased) union of two maps. It prefers the first map when
-- duplicate keys are encountered, i.e. @'union' == 'unionWith' const@.
union :: TypeRepMap f -> TypeRepMap f -> TypeRepMap f
union = unionWith const
{-# INLINE union #-}

{- | Returns 'True' if there exist value of given type.

>>> member @Char $ one (Identity 'a')
True
>>> member @Bool $ one (Identity 'a')
False
-}
member :: forall a (f :: KindOf a -> Type) . Typeable a => TypeRepMap f -> Bool
member trMap = case lookup @a trMap of
    Nothing -> False
    Just _  -> True
{-# INLINE member #-}

{- | Looks up the value at the type.

>>> let x = lookup $ insert (Identity (11 :: Int)) empty
>>> x :: Maybe (Identity Int)
Just (Identity 11)
>>> x :: Maybe (Identity ())
Nothing
-}
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
lookup tVect = fromAny . (anys tVect `indexArray`)
           <$> cachedBinarySearch (typeFp @a)
                                  (fingerprintAs tVect)
                                  (fingerprintBs tVect)
{-# INLINE lookup #-}

-- | Returns the size of the 'TypeRepMap'.
size :: TypeRepMap f -> Int
size = sizeofPrimArray . fingerprintAs
{-# INLINE size #-}

-- | Binary searched based on this article
-- http://bannalia.blogspot.com/2015/06/cache-friendly-binary-search.html
-- with modification for our two-vector search case.
cachedBinarySearch :: Fingerprint -> PrimArray Word64 -> PrimArray Word64 -> Maybe Int
cachedBinarySearch (Fingerprint (W64# a) (W64# b)) fpAs fpBs = inline (go 0#)
  where
    go :: Int# -> Maybe Int
    go i = case i <# len of
        0# -> Nothing
        _  -> let !(W64# valA) = indexPrimArray fpAs (I# i) in case a `ltWord#` valA of
            0#  -> case a `eqWord#` valA of
                0# -> go (2# *# i +# 2#)
                _ -> let !(W64# valB) = indexPrimArray fpBs (I# i) in case b `eqWord#` valB of
                    0# -> case b `ltWord#` valB of
                        0# -> go (2# *# i +# 2#)
                        _  -> go (2# *# i +# 1#)
                    _ -> Just (I# i)
            _ -> go (2# *# i +# 1#)

    len :: Int#
    len = let !(I# l) = sizeofPrimArray fpAs in l
{-# INLINE cachedBinarySearch #-}

----------------------------------------------------------------------------
-- Internal functions
----------------------------------------------------------------------------

toAny :: f a -> Any
toAny = unsafeCoerce

fromAny :: Any -> f a
fromAny = unsafeCoerce

typeFp :: forall a . Typeable a => Fingerprint
typeFp = typeRepFingerprint $ typeRep $ Proxy @a
{-# INLINE typeFp #-}

toPairList :: TypeRepMap f -> [(Fingerprint, Any)]
toPairList trMap = zip (toFingerprints trMap) (GHC.toList $ anys trMap)

deleteByFst :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteByFst x = filter ((/= x) . fst)

nubByFst :: (Eq a) => [(a, b)] -> [(a, b)]
nubByFst = nubBy ((==) `on` fst)

----------------------------------------------------------------------------
-- Functions for testing and benchmarking
----------------------------------------------------------------------------

-- | Existential wrapper around 'Typeable' indexed by @f@ type parameter.
-- Useful for 'TypeRepMap' structure creation form list of 'TF's.
data TF f where
    TF :: Typeable a => f a -> TF f

instance Show (TF f) where
    show (TF tf) = show $ calcFp tf

{- | Creates 'TypeRepMap' from a list of 'TF's.

>>> size $ fromList [TF $ Identity True, TF $ Identity 'a']
2

-}
fromList :: forall f . [TF f] -> TypeRepMap f
fromList = fromListPairs . map (fp &&& an)
  where
    fp :: TF f -> Fingerprint
    fp (TF x) = calcFp x

    an :: TF f -> Any
    an (TF x) = toAny x

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

calcFp :: Typeable a => f a -> Fingerprint
calcFp = typeRepFingerprint . typeRep . fromF

fromListPairs :: [(Fingerprint, Any)] -> TypeRepMap f
fromListPairs kvs = TypeRepMap (GHC.fromList fpAs) (GHC.fromList fpBs) (GHC.fromList ans)
  where
    (fpAs, fpBs) = unzip $ map (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans) = unzip $ fromSortedList $ sortWith fst $ nubByFst kvs

----------------------------------------------------------------------------
-- Tree-like conversion
----------------------------------------------------------------------------

fromSortedList :: forall a . [a] -> [a]
fromSortedList l = IM.elems $ fst $ go 0 0 mempty (IM.fromList $ zip [0..] l)
  where
    -- state monad could be used here, but it's another dependency
    go :: Int -> Int -> IntMap a -> IntMap a -> (IntMap a, Int)
    go i first result vector =
      if i >= IM.size vector
      then (result, first)
      else do
          let (newResult, newFirst) = go (2 * i + 1) first result vector
          let withCur = IM.insert i (fromJust $ IM.lookup newFirst vector) newResult
          go (2 * i + 2) (newFirst + 1) withCur vector
