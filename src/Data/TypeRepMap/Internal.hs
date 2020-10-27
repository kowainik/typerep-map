{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE ViewPatterns          #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes #-}

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Internal API for 'TypeRepMap' and operations on it. The functions here do
not have any stability guarantees and can change between minor versions.

If you need to use this module for purposes other than tests,
create an issue.
-}

#include "MachDeps.h"

module Data.TypeRepMap.Internal where

import Prelude hiding (lookup)

import Control.DeepSeq
import Control.Monad.ST (ST, runST)
import Control.Monad.Zip (mzip)
import Data.Function (on)
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe)
import Data.Primitive.Array (Array, MutableArray, indexArray, mapArray', sizeofArray,
                             thawArray, unsafeFreezeArray, writeArray)
import Data.Primitive.PrimArray (writePrimArray, newPrimArray, unsafeFreezePrimArray, primArrayToList, MutablePrimArray, primArrayFromListN, PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Semigroup (Semigroup (..), All(..))
import GHC.Base (Any, Int (..), Int#, (*#), (+#), (<#))
import GHC.Exts (IsList (..), inline, sortWith)
import GHC.Fingerprint (Fingerprint (..))
#if WORD_SIZE_IN_BITS >= 64
import GHC.Prim (eqWord#, ltWord#)
#else
import GHC.IntWord64 (eqWord64#, ltWord64#)
#define eqWord eqWord64
#define ltWord ltWord64
#endif
import GHC.Word (Word64 (..))
import Type.Reflection (SomeTypeRep (..), TypeRep, Typeable, typeRep, withTypeable)
import Type.Reflection.Unsafe (typeRepFingerprint)
import Unsafe.Coerce (unsafeCoerce)

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
    , trAnys        :: {-# UNPACK #-} !(Array Any)        -- ^ values stored in the map
    , trKeys        :: {-# UNPACK #-} !(Array Any)        -- ^ typerep keys
    }
  -- ^ an unsafe constructor for 'TypeRepMap'

instance NFData (TypeRepMap f) where
   rnf x = rnf (keys x) `seq` ()

-- | Shows only keys.
instance Show (TypeRepMap f) where
    show TypeRepMap{..} = "TypeRepMap [" ++ showKeys ++ "]"
      where
        showKeys :: String
        showKeys = intercalate ", " $ toList $ mapArray' (show . anyToTypeRep) trKeys

-- | Uses 'union' to combine 'TypeRepMap's.
instance Semigroup (TypeRepMap f) where
    (<>) :: TypeRepMap f -> TypeRepMap f -> TypeRepMap f
    (<>) = union
    {-# INLINE (<>) #-}

instance Monoid (TypeRepMap f) where
    mempty = TypeRepMap mempty mempty mempty mempty
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

#if __GLASGOW_HASKELL__ >= 806
instance (forall a. Typeable a => Eq (f a)) => Eq (TypeRepMap f) where
    tm1 == tm2 = size tm1 == size tm2 && go 0
      where
        go :: Int -> Bool
        go i
            | i == size tm1 = True
            | otherwise = case testEquality tr1i tr2i of
                  Nothing -> False
                  Just Refl -> repEq tr1i (fromAny tv1i) (fromAny tv2i) && go (i + 1)
          where
            tr1i :: TypeRep x
            tr1i = anyToTypeRep $ indexArray (trKeys tm1) i

            tr2i :: TypeRep y
            tr2i = anyToTypeRep $ indexArray (trKeys tm2) i

            tv1i, tv2i :: Any
            tv1i = indexArray (trAnys tm1) i
            tv2i = indexArray (trAnys tm2) i

            repEq :: TypeRep x -> f x -> f x -> Bool
            repEq tr = withTypeable tr (==)
#endif

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
one x = TypeRepMap (primArrayFromListN 1 [fa])
                   (primArrayFromListN 1 [fb])
                   (pure @Array v)
                   (pure @Array k)
  where
    (Fingerprint fa fb, v, k) = (calcFp @a, toAny x, unsafeCoerce $ typeRep @a)
{-# INLINE one #-}

{- |

Insert a value into a 'TypeRepMap'.
TypeRepMap optimizes for fast reads rather than inserts, as a trade-off inserts are @O(n)@.

prop> size (insert v tm) >= size tm
prop> member @a (insert (x :: f a) tm) == True

-}
insert :: forall a f . Typeable a => f a -> TypeRepMap f -> TypeRepMap f
insert x m
  | size m == 0 = one x
  | otherwise = case cachedBinarySearch (typeFp @a) (fingerprintAs m) (fingerprintBs m) of
      Nothing -> union m $ one x
      Just i -> m {trAnys = changeAnyArr i (trAnys m)}
  where
    changeAnyArr :: Int -> Array Any -> Array Any
    changeAnyArr i trAs = runST $ do
      let n = sizeofArray trAs
      mutArr <- thawArray trAs 0 n
      writeArray mutArr i $ toAny x
      unsafeFreezeArray mutArr
{-# INLINE insert #-}

-- Extract the kind of a type. We use it to work around lack of syntax for
-- inferred type variables (which are not subject to type applications).
type KindOf (a :: k) = k

{- | Delete a value from a 'TypeRepMap'.

TypeRepMap optimizes for fast reads rather than modifications, as a trade-off deletes are 
@O(n)@, with an @O(log(n))@ optimization for when the element is already missing.

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
delete m
  -- Lookups are fast, so check if we even have the element first.
  | not (member @a m) = m
  -- We know we have the element, If the map has exactly one element, we can return the empty map
  | size m == 1 = empty
  -- Otherwise, filter out the element in linear time.
  | otherwise = fromSortedTriples . deleteFirst ((== typeFp @a) . fst3) . toSortedTriples $ m
{-# INLINE delete #-}

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst p (x : xs) = if p x then xs else x : deleteFirst p xs

{- |
Update a value at a specific key with the result of the provided function. 
When the key is not a member of the map, the original map is returned.

>>> trmap = fromList @(TypeRepMap Identity) [WrapTypeable $ Identity "a"]
>>> lookup @String $ adjust (fmap (++ "ww")) trmap
Just (Identity "aww")
-}
adjust :: forall a f . Typeable a => (f a -> f a) -> TypeRepMap f -> TypeRepMap f
adjust fun = alter (fmap fun)
{-# INLINE adjust #-}

{- |
Updates a value at a specific key, whether or not it exists.
This can be used to insert, delete, or update a value of a given type in the map.

>>> func = (\case Nothing -> Just (Identity "new"); Just (Identity s) -> Just (Identity (reverse s)))
>>> lookup @String $ alter func empty
Just (Identity "new")
>>> trmap = fromList @(TypeRepMap Identity) [WrapTypeable $ Identity "helllo"]
>>> lookup @String $ alter func trmap
>>> Just (Identity "olleh")
-}
alter :: forall a f . Typeable a => (Maybe (f a) -> Maybe (f a)) -> TypeRepMap f -> TypeRepMap f
alter fun tr = case cachedBinarySearch (typeFp @a) (fingerprintAs tr) (fingerprintBs tr) of
    Nothing ->
        case (fun Nothing) of
            Nothing -> tr
            Just v -> insert v tr
    Just i  ->
        case fun (Just . fromAny $ indexArray (trAnys tr) i) of
            Nothing -> delete @a tr
            Just v -> tr{trAnys = replaceAnyAt i (toAny v) (trAnys tr)}
  where
    replaceAnyAt :: Int -> Any -> Array Any -> Array Any
    replaceAnyAt i v trAs = runST $ do
        let n = sizeofArray trAs
        mutArr <- thawArray trAs 0 n
        writeArray mutArr i v
        unsafeFreezeArray mutArr
{-# INLINE alter #-}

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
hoist f (TypeRepMap as bs ans ks) = TypeRepMap as bs (mapArray' (toAny . f . fromAny) ans) ks
{-# INLINE hoist #-}

hoistA :: (Applicative t) => (forall x. f x -> t (g x)) -> TypeRepMap f -> t (TypeRepMap g)
hoistA f (TypeRepMap as bs (toList -> ans) ks) = (\l -> TypeRepMap as bs (fromList $ map toAny l) ks)
    <$> traverse (f . fromAny) ans
{-# INLINE hoistA #-}

hoistWithKey :: forall f g. (forall x. Typeable x => f x -> g x) -> TypeRepMap f -> TypeRepMap g
hoistWithKey f (TypeRepMap as bs ans ks) = TypeRepMap as bs newAns ks
  where
    newAns = mapArray' mapAns (mzip ans ks)
    mapAns (a, k) = toAny $ withTr (unsafeCoerce k) $ fromAny a

    withTr :: forall x. TypeRep x -> f x -> g x
    withTr t = withTypeable t f
{-# INLINE hoistWithKey #-}

-- | The union of two 'TypeRepMap's using a combining function for conflicting entries. @O(n + m)@
unionWith :: forall f. (forall x. Typeable x => f x -> f x -> f x) -> TypeRepMap f -> TypeRepMap f -> TypeRepMap f
unionWith f ma mb = do
    fromSortedTriples $ mergeMaps (toSortedTriples ma) (toSortedTriples mb)
  where
    f' :: forall x. TypeRep x -> f x -> f x -> f x
    f' tr = withTypeable tr f

    combine :: (Fingerprint, Any, Any) -> (Fingerprint, Any, Any) -> (Fingerprint, Any, Any)
    combine (fp, av, ak) (_, bv, _) = (fp, toAny $ f' (fromAny ak) (fromAny av) (fromAny bv), ak)

    -- Merges two typrepmaps into a sorted, dedup'd list of triples.
    -- Using 'toSortedTriples' allows us to assume the triples are sorted by fingerprint,
    -- Given O(n) performance from 'toSortedTriples', and given that we can merge-sort in
    -- O(n + m) time, then can '.fromSortedTriples' back into cachedBinarySearch order in O(n + m)
    -- that gives a total of O(n + m).
    mergeMaps :: [(Fingerprint, Any, Any)] -> [(Fingerprint, Any, Any)] -> [(Fingerprint, Any, Any)]
    -- We've addressed all elements from both maps
    mergeMaps as [] = as
    mergeMaps [] bs = bs
    -- Merge
    mergeMaps (a@(af, _, _) : as) (b@(bf, _, _) : bs) =
        case compare af bf of
            -- Fingerprints are equal, union the elements using our function
            -- If the incoming maps were de-duped, there shouldn't be any other equivalent
            -- fingerprints
            EQ -> combine a b : mergeMaps as bs
            -- First fingerprint must not be in the second map or we would have seen it by now
            -- Add it to the result as-is
            LT -> a : mergeMaps as (b : bs)
            -- Second fingerprint must not be in the first map or we would have seen it by now
            -- Add it to the result as-is
            GT -> b : mergeMaps (a:as) bs
{-# INLINE unionWith #-}

-- | The (left-biased) union of two 'TypeRepMap's in @O(n + m)@. It prefers the first map when
-- duplicate keys are encountered, i.e. @'union' == 'unionWith' const@.
union :: TypeRepMap f -> TypeRepMap f -> TypeRepMap f
union = unionWith const
{-# INLINE union #-}

-- | The 'intersection' of two 'TypeRepMap's using a combining function
--
-- @O(n + m)@
intersectionWith :: forall f. (forall x. Typeable x => f x -> f x -> f x) -> TypeRepMap f -> TypeRepMap f -> TypeRepMap f
intersectionWith f ma mb =
    fromSortedTriples $ mergeMaps (toSortedTriples ma) (toSortedTriples mb)
  where
    f' :: forall x. TypeRep x -> f x -> f x -> f x
    f' tr = withTypeable tr f

    combine :: (Fingerprint, Any, Any) -> (Fingerprint, Any, Any) -> (Fingerprint, Any, Any)
    combine (fp, av, ak) (_, bv, _) = (fp, toAny $ f' (fromAny ak) (fromAny av) (fromAny bv), ak)

    -- Merges two typrepmaps into a sorted, dedup'd list of triples.
    mergeMaps :: [(Fingerprint, Any, Any)] -> [(Fingerprint, Any, Any)] -> [(Fingerprint, Any, Any)]
    -- If either list is empty, the intersection must be finished.
    mergeMaps _ [] = []
    mergeMaps [] _ = []
    -- Merge the two maps considering one element at a time.
    mergeMaps (a@(af, _, _) : as) (b@(bf, _, _) : bs) = 
        case compare af bf of
            -- Fingerprints are equal, union the elements using our function
            -- If the incoming maps were de-duped, there shouldn't be any other equivalent
            -- fingerprints
            EQ -> combine a b : mergeMaps as bs
            -- First fingerprint must not be in the second map or we would have seen it by now
            -- Skip it an move on
            LT -> mergeMaps as (b : bs)
            -- Second fingerprint must not be in the first map or we would have seen it by now
            -- Skip it an move on
            GT -> mergeMaps (a:as) bs
{-# INLINE intersectionWith #-}

-- | The intersection of two 'TypeRepMap's. 
-- It keeps all values from the first map whose keys are present in the second.
--
-- @O(n + m)@
intersection :: TypeRepMap f -> TypeRepMap f -> TypeRepMap f
intersection = intersectionWith const
{-# INLINE intersection #-}


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
lookup tVect = fromAny . (trAnys tVect `indexArray`)
           <$> cachedBinarySearch (typeFp @a)
                                  (fingerprintAs tVect)
                                  (fingerprintBs tVect)
{-# INLINE lookup #-}

-- | Get the amount of elements in a 'TypeRepMap'.
size :: TypeRepMap f -> Int
size = sizeofPrimArray . fingerprintAs
{-# INLINE size #-}

-- | Return the list of 'SomeTypeRep' from the keys.
keys :: TypeRepMap f -> [SomeTypeRep]
keys TypeRepMap{..} = SomeTypeRep . anyToTypeRep <$> toList trKeys
{-# INLINE keys #-}

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

anyToTypeRep :: Any -> TypeRep f
anyToTypeRep = unsafeCoerce

typeFp :: forall a . Typeable a => Fingerprint
typeFp = typeRepFingerprint $ typeRep @a
{-# INLINE typeFp #-}

toTriples :: TypeRepMap f -> [(Fingerprint, Any, Any)]
toTriples tm = zip3 (toFingerprints tm) (GHC.toList $ trAnys tm) (GHC.toList $ trKeys tm)

-- | Efficiently get sorted triples from a map in O(n) time
--
-- We assume the incoming TypeRepMap is already sorted into 'cachedBinarySearch' order using fromSortedList.
-- Then we can construct the index mapping from the "cached" ordering into monotonically 
-- increasing order using 'generateOrderMapping' with the length of the TRM. This takes @O(n).
-- We then pull those indexes from the source TRM to get the sorted triples in a total of @O(n).
toSortedTriples :: TypeRepMap f -> [(Fingerprint, Any, Any)]
toSortedTriples tm = trip <$> ordering
  where
    trip i = ( Fingerprint (indexPrimArray (fingerprintAs tm) i) (indexPrimArray (fingerprintBs tm) i)
             , indexArray (trAnys tm) i
             , indexArray (trKeys tm) i)
    ordering :: [ Int ]
    ordering = generateOrderMapping (size tm)

nubByFst :: (Eq a) => [(a, b, c)] -> [(a, b, c)]
nubByFst = nubBy ((==) `on` fst3)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

----------------------------------------------------------------------------
-- Functions for testing and benchmarking
----------------------------------------------------------------------------

-- | Existential wrapper around 'Typeable' indexed by @f@ type parameter.
-- Useful for 'TypeRepMap' structure creation form list of 'WrapTypeable's.
data WrapTypeable f where
    WrapTypeable :: Typeable a => f a -> WrapTypeable f

instance Show (WrapTypeable f) where
    show (WrapTypeable (_ :: f a)) = show $ calcFp @a

wrapTypeable :: TypeRep a -> f a -> WrapTypeable f
wrapTypeable tr = withTypeable tr WrapTypeable

{- |

prop> fromList . toList == 'id'

Creates 'TypeRepMap' from a list of 'WrapTypeable's.

>>> show $ fromList [WrapTypeable $ Identity True, WrapTypeable $ Identity 'a']
TypeRepMap [Bool, Char]


-}
instance IsList (TypeRepMap f) where
    type Item (TypeRepMap f) = WrapTypeable f

    fromList :: [WrapTypeable f] -> TypeRepMap f
    fromList = fromTriples . map (\x -> (fp x, an x, k x))
      where
        fp :: WrapTypeable f -> Fingerprint
        fp (WrapTypeable (_ :: f a)) = calcFp @a

        an :: WrapTypeable f -> Any
        an (WrapTypeable x) = toAny x

        k :: WrapTypeable f -> Any
        k (WrapTypeable (_ :: f a)) = unsafeCoerce $ typeRep @a

    toList :: TypeRepMap f -> [WrapTypeable f]
    toList = map toWrapTypeable . toTriples
      where
        toWrapTypeable :: (Fingerprint, Any, Any) -> WrapTypeable f
        toWrapTypeable (_, an, k) = wrapTypeable (unsafeCoerce k) (fromAny an)

calcFp :: forall a . Typeable a => Fingerprint
calcFp = typeRepFingerprint $ typeRep @a

fromTriples :: [(Fingerprint, Any, Any)] -> TypeRepMap f
fromTriples = fromSortedTriples . sortWith fst3 . nubByFst

fromSortedTriples :: [(Fingerprint, Any, Any)] -> TypeRepMap f
fromSortedTriples kvs = TypeRepMap (GHC.fromList fpAs) (GHC.fromList fpBs) (GHC.fromList ans) (GHC.fromList ks)
  where
    (fpAs, fpBs) = unzip $ map (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans, ks) = unzip3 $ fromSortedList kvs

----------------------------------------------------------------------------
-- Tree-like conversion
----------------------------------------------------------------------------

fromSortedList :: forall a . [a] -> [a]
fromSortedList l = runST $ do
    let n = length l
    let arrOrigin = fromListN n l
    arrResult <- thawArray arrOrigin 0 n
    go n arrResult arrOrigin
    toList <$> unsafeFreezeArray arrResult
  where
    -- state monad could be used here, but it's another dependency
    go :: forall s . Int -> MutableArray s a -> Array a -> ST s ()
    go len result origin = () <$ loop 0 0
      where
        loop :: Int -> Int -> ST s Int
        loop i first =
            if i >= len
            then pure first
            else do
                newFirst <- loop (2 * i + 1) first
                writeArray result i (indexArray origin newFirst)
                loop (2 * i + 2) (newFirst + 1)

-- Returns a list of indexes which represents the "sorted" order of an array generated by
-- fromSortedList of the provided length.
-- I.e. fmap (fromSortedList [1, 2, 3, 4, 5, 6] !!) (generateOrderMapping 6) == [1, 2, 3, 4, 5, 6]
--
--     >>> generateOrderMapping 6
--     [3,1,4,0,5,2]
--
--     >>> generateOrderMapping 8
--     [7,3,1,4,0,5,2,6]
generateOrderMapping :: Int -> [Int]
generateOrderMapping len = runST $ do
    orderMappingArr <- newPrimArray len
    _ <- loop orderMappingArr 0 0
    primArrayToList <$> unsafeFreezePrimArray orderMappingArr
  where
    loop :: MutablePrimArray s Int -> Int -> Int -> ST s Int
    loop result i first =
        if i >= len
        then pure first
        else do
            newFirst <- loop result (2 * i + 1) first
            writePrimArray result newFirst i
            loop result (2 * i + 2) (newFirst + 1)

----------------------------------------------------------------------------
--  Helper functions.
----------------------------------------------------------------------------

-- | Check that invariant of the structure holds.
-- The structure maintains the following invariant.
-- For each element @A@ at index @i@:
--
--   1. if there is an element @B@ at index @2*i+1@,
--      then @B < A@.
--
--   2. if there is an element @C@ at index @2*i+2@,
--      then @A < C@.
--
invariantCheck :: TypeRepMap f -> Bool
invariantCheck TypeRepMap{..} = getAll (check 0)
  where
    lastMay [] = Nothing
    lastMay [x] = Just x
    lastMay (_:xs) = lastMay xs
    sz = sizeofPrimArray fingerprintAs
    check i | i >= sz = All True
            | otherwise =
      let left = i*2+1
          right = i*2+2
          -- maximum value in the left branch
          leftMax =
               fmap (\j -> (indexPrimArray fingerprintAs j, indexPrimArray fingerprintBs j))
             $ lastMay
             $ takeWhile (<sz)
             $ iterate (\j -> j*2+2) left
          -- minimum value in the right branch
          rightMin =
               fmap (\j -> (indexPrimArray fingerprintAs j, indexPrimArray fingerprintBs j))
             $ lastMay
             $ takeWhile (<sz)
             $ iterate (\j -> j*2+1) right
      in mconcat
          [ All $
            if left < sz
            then
              case indexPrimArray fingerprintAs i `compare` indexPrimArray fingerprintAs left of
                LT -> False
                EQ -> indexPrimArray fingerprintBs i >= indexPrimArray fingerprintBs left
                GT -> True
            else True
         , All $
           if right < sz
           then
              case indexPrimArray fingerprintAs i `compare` indexPrimArray fingerprintAs right of
                LT -> True
                EQ -> indexPrimArray fingerprintBs i <= indexPrimArray fingerprintBs right
                GT -> False
            else True
         , All $ fromMaybe True $ (<=) <$> leftMax <*> rightMin
         , check (i+1)
         ]
