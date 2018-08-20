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
{-# LANGUAGE ViewPatterns        #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes #-}

-- | Internal API for 'TypeRepMap' and operations on it. The functions here do
-- not have any stability guarantees and can change between minor versions.
--
-- If you need to use this module for purposes other than tests,
-- create an issue.
--
module Data.TypeRepMap.Internal where

import Prelude hiding (lookup)

import Control.Monad.ST (ST, runST)
import Control.Monad.Zip (mzip)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (intercalate, nubBy)
import Data.Primitive.Array (Array, MutableArray, indexArray, mapArray', readArray, sizeofArray,
                             thawArray, unsafeFreezeArray, writeArray)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, sizeofPrimArray)
import Data.Semigroup (Semigroup (..))
import GHC.Base (Any, Int (..), Int#, (*#), (+#), (<#))
import GHC.Exts (IsList (..), inline, sortWith)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Prim (eqWord#, ltWord#)
import GHC.Word (Word64 (..))
import Type.Reflection (SomeTypeRep (..), TypeRep, Typeable, typeRep, withTypeable)
import Type.Reflection.Unsafe (typeRepFingerprint)
import Unsafe.Coerce (unsafeCoerce)

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
    , trAnys        :: {-# UNPACK #-} !(Array Any)        -- ^ values stored in the map
    , trKeys        :: {-# UNPACK #-} !(Array Any)        -- ^ typerep keys
    }
  -- ^ an unsafe constructor for 'TypeRepMap'

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
insert x = fromTriples . addX . toTriples
  where
    tripleX :: (Fingerprint, Any, Any)
    tripleX@(fpX, _, _) = (calcFp @a, toAny x, unsafeCoerce $ typeRep @a)

    addX :: [(Fingerprint, Any, Any)] -> [(Fingerprint, Any, Any)]
    addX l = tripleX : deleteByFst fpX l
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
delete = fromTriples . deleteByFst (typeFp @a) . toTriples
{-# INLINE delete #-}

{- |
Update a value at a specific key with the result of the provided function. When
the key is not a member of the map, the original map is returned.

>>> trmap = fromList @(TypeRepMap Identity) [WrapTypeable $ Identity "a"]
>>> lookup @String $ adjust (fmap (++ "ww")) trmap
Just (Identity "aww")
-}
adjust :: forall a f . Typeable a => (f a -> f a) -> TypeRepMap f -> TypeRepMap f
adjust fun tr = case cachedBinarySearch (typeFp @a) (fingerprintAs tr) (fingerprintBs tr) of
    Nothing -> tr
    Just i  -> tr {trAnys = changeAnyArr i (trAnys tr)}
  where
    changeAnyArr :: Int -> Array Any -> Array Any
    changeAnyArr i trAs = runST $ do
        let n = sizeofArray trAs
        mutArr <- thawArray trAs 0 n
        a <- toAny . fun . fromAny <$> readArray mutArr i
        writeArray mutArr i a
        unsafeFreezeArray mutArr
{-# INLINE adjust #-}

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

-- | The union of two 'TypeRepMap's using a combining function.
unionWith :: (forall x. f x -> f x -> f x) -> TypeRepMap f -> TypeRepMap f -> TypeRepMap f
unionWith f m1 m2 = fromTriples
                  $ toTripleList
                  $ Map.unionWith combine
                                  (fromTripleList $ toTriples m1)
                                  (fromTripleList $ toTriples m2)
  where
    combine :: (Any, Any) -> (Any, Any) -> (Any, Any)
    combine (av, ak) (bv, _) = (toAny $ f (fromAny av) (fromAny bv), ak)

    fromTripleList :: Ord a => [(a, b, c)] -> Map.Map a (b, c)
    fromTripleList = Map.fromList . map (\(a, b, c) -> (a, (b, c)))

    toTripleList :: Map.Map a (b, c) -> [(a, b, c)]
    toTripleList = map (\(a, (b, c)) -> (a, b, c)) . Map.toList
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

deleteByFst :: Eq a => a -> [(a, b, c)] -> [(a, b, c)]
deleteByFst x = filter ((/= x) . fst3)

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
        toWrapTypeable (_, an, k) = withTypeable (unsafeCoerce k) $ fromAny an

calcFp :: forall a . Typeable a => Fingerprint
calcFp = typeRepFingerprint $ typeRep @a

fromTriples :: [(Fingerprint, Any, Any)] -> TypeRepMap f
fromTriples kvs = TypeRepMap (GHC.fromList fpAs) (GHC.fromList fpBs) (GHC.fromList ans) (GHC.fromList ks)
  where
    (fpAs, fpBs) = unzip $ map (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans, ks) = unzip3 $ fromSortedList $ sortWith fst3 $ nubByFst kvs

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
