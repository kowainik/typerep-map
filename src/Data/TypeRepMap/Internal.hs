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

-- | Internal API for 'TypeRepMap' and operations on it. The functions here do
-- not have any stability guarantees and can change between minor versions.
--
-- If you need to use this module for purposes other than tests,
-- create an issue.
--
module Data.TypeRepMap.Internal where

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
import GHC.Exts (IsList (..), inline, sortWith)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Prim (eqWord#, ltWord#)
import GHC.Word (Word64 (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified GHC.Exts as GHC (fromList, toList)

{- |

'TypeRepMap' is a heterogeneous data structure similar in its essence to
'Data.Map.Map' with types as keys, where each value has the type of its key. In
addition to that, each value is wrapped in an interpretation @f@.

Here is an example of using 'Prelude.Maybe' as an interpretation, with a
comparison to 'Data.Map.Map':

@
 'Data.Map.Map' 'Prelude.String' ('Prelude.Maybe' 'Prelude.String')          'TypeRepMap' 'Prelude.Maybe'
---------------------------       ---------------------
 \"Int\"  -> Just \"5\"                 'Prelude.Int'  -> Just 5
 \"Bool\" -> Just \"True\"              'Prelude.Bool' -> Just 'Prelude.True'
 \"Char\" -> Nothing                  'Prelude.Char' -> Nothing
@

The runtime representation of 'TypeRepMap' is an array, not a tree. This makes
'lookup' significantly more efficient.

-}
data TypeRepMap (f :: k -> Type) =
  TypeRepMap
    { fingerprintAs :: {-# UNPACK #-} !(PrimArray Word64) -- ^ first components of key fingerprints
    , fingerprintBs :: {-# UNPACK #-} !(PrimArray Word64) -- ^ second components of key fingerprints
    , anys          :: {-# UNPACK #-} !(Array Any)        -- ^ values stored in the map
    }
  -- ^ an unsafe constructor for 'TypeRepMap'

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

{- |

A 'TypeRepMap' with no values stored in it.

prop> size empty == 0
prop> member @a empty == False

-}
empty :: TypeRepMap f
empty = mempty
{-# INLINE empty #-}

{- |

Construct a 'TypeRepMap' with a single element.

prop> size (one x) == 1
prop> member @a (one (x :: f a)) == True

-}
one :: forall a f . Typeable a => f a -> TypeRepMap f
one x = insert x empty
{-# INLINE one #-}

{- |

Insert a value into a 'TypeRepMap'.

prop> size (insert v tm) >= size tm
prop> member @a (insert (x :: f a) tm) == True

-}
insert :: forall a f . Typeable a => f a -> TypeRepMap f -> TypeRepMap f
insert x = fromListPairs . addX . toPairList
  where
    pairX :: (Fingerprint, Any)
    pairX@(fpX, _) = (calcFp x, toAny x)

    addX :: [(Fingerprint, Any)] -> [(Fingerprint, Any)]
    addX l = pairX : deleteByFst fpX l
{-# INLINE insert #-}

-- Extract the kind of a type. We use it to work around lack of syntax for
-- inferred type variables (which are not subject to type applications).
type KindOf (a :: k) = k

{- | Delete a value from a 'TypeRepMap'.

prop> size (delete @a tm) <= size tm
prop> member @a (delete @a tm) == False

>>> tm = delete @Bool $ insert (Just True) $ one (Just 'a')
>>> size tm
1
>>> member @Bool tm
False
>>> member @Char tm
True
-}
delete :: forall a (f :: KindOf a -> Type) . Typeable a => TypeRepMap f -> TypeRepMap f
delete = fromListPairs . deleteByFst (typeFp @a) . toPairList
{-# INLINE delete #-}

{- | Map over the elements of a 'TypeRepMap'.

>>> tm = insert (Identity True) $ one (Identity 'a')
>>> lookup @Bool tm
Just (Identity True)
>>> lookup @Char tm
Just (Identity 'a')
>>> tm2 = hoist ((:[]) . runIdentity) tm
>>> lookup @Bool tm2
Just [True]
>>> lookup @Char tm2
Just "a"
-}
hoist :: (forall x. f x -> g x) -> TypeRepMap f -> TypeRepMap g
hoist f (TypeRepMap as bs ans) = TypeRepMap as bs $ mapArray' (toAny . f . fromAny) ans
{-# INLINE hoist #-}

-- | The union of two 'TypeRepMap's using a combining function.
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

-- | The (left-biased) union of two 'TypeRepMap's. It prefers the first map when
-- duplicate keys are encountered, i.e. @'union' == 'unionWith' const@.
union :: TypeRepMap f -> TypeRepMap f -> TypeRepMap f
union = unionWith const
{-# INLINE union #-}

{- | Check if a value of the given type is present in a 'TypeRepMap'.

>>> member @Char $ one (Identity 'a')
True
>>> member @Bool $ one (Identity 'a')
False
-}
member :: forall a (f :: KindOf a -> Type) . Typeable a => TypeRepMap f -> Bool
member tm = case lookup @a tm of
    Nothing -> False
    Just _  -> True
{-# INLINE member #-}

{- | Lookup a value of the given type in a 'TypeRepMap'.

>>> x = lookup $ insert (Identity (11 :: Int)) empty
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

-- | Get the amount of elements in a 'TypeRepMap'.
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
toPairList tm = zip (toFingerprints tm) (GHC.toList $ anys tm)

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

{- |

prop> fromList . toList == 'id'

Creates 'TypeRepMap' from a list of 'TF's.

>>> size $ fromList [TF $ Identity True, TF $ Identity 'a']
2


-}
instance IsList (TypeRepMap f) where
    type Item (TypeRepMap f) = TF f

    fromList :: forall p . [TF p] -> TypeRepMap p
    fromList = fromListPairs . map (fp &&& an)
      where
        fp :: TF p -> Fingerprint
        fp (TF x) = calcFp x

        an :: TF p -> Any
        an (TF x) = toAny x

    toList :: forall p . TypeRepMap p -> [TF p]
    toList TypeRepMap{..} = undefined

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
