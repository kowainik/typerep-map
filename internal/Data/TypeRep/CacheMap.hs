{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}

-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes #-}

module Data.TypeRep.CacheMap
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
       , cachedBinarySearch
       ) where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Word (Word64)
import GHC.Base (Any, Int (..), Int#, (*#), (+#), (<#))
import GHC.Exts (inline, sortWith)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Prim (eqWord#, ltWord#)
import GHC.Word (Word64 (..))
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
insert :: forall a f . Typeable a => f a -> TypeRepMap f -> TypeRepMap f
insert x = fromListPairs . addX . toPairList
  where
    toPairList :: TypeRepMap f -> [(Fingerprint, Any)]
    toPairList (TypeRepMap as bs ans) = zip (zipWith Fingerprint
                                                    (Unboxed.toList as)
                                                    (Unboxed.toList bs))
                                            (V.toList ans)

    pairX :: (Fingerprint, Any)
    pairX@(fpX, _) = (calcFp x, unsafeCoerce x)

    addX :: [(Fingerprint, Any)] -> [(Fingerprint, Any)]
    addX l = pairX : filter ((/= fpX) . fst) l
{-# INLINE insert #-}

-- | Looks up the value at the type.
-- >>> let x = lookup $ insert (11 :: Int) empty
-- >>> x :: Maybe Int
-- Just 11
-- >>> x :: Maybe ()
-- Nothing
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
lookup tVect = fromAny . (anys tVect V.!)
           <$> cachedBinarySearch (typeRepFingerprint $ typeRep $ Proxy @a)
                                  (fingerprintAs tVect)
                                  (fingerprintBs tVect)
{-# INLINE lookup #-}

-- | Returns the size of the 'TypeRepMap'.
size :: TypeRepMap f -> Int
size = Unboxed.length . fingerprintAs

-- | Binary searched based on this article
-- http://bannalia.blogspot.com/2015/06/cache-friendly-binary-search.html
-- with modification for our two-vector search case.
cachedBinarySearch :: Fingerprint -> Unboxed.Vector Word64 -> Unboxed.Vector Word64 -> Maybe Int
cachedBinarySearch (Fingerprint (W64# a) (W64# b)) fpAs fpBs = inline (go 0#)
  where
    go :: Int# -> Maybe Int
    go i = case i <# len of
        0# -> Nothing
        _  -> let !(W64# valA) = Unboxed.unsafeIndex fpAs (I# i) in case a `ltWord#` valA of
            0#  -> case a `eqWord#` valA of
                0# -> go (2# *# i +# 2#)
                _ -> let !(W64# valB) = Unboxed.unsafeIndex fpBs (I# i) in case b `eqWord#` valB of
                    0# -> case b `ltWord#` valB of
                        0# -> go (2# *# i +# 2#)
                        _  -> go (2# *# i +# 1#)
                    _ -> Just (I# i)
            _ -> go (2# *# i +# 1#)

    len :: Int#
    len = let !(I# l) = Unboxed.length fpAs in l
{-# INLINE cachedBinarySearch #-}

----------------------------------------------------------------------------
-- Functions for testing and benchmarking
----------------------------------------------------------------------------

data TF f where
  TF :: Typeable a => f a -> TF f

instance Eq (TF f) where
    (TF f1) == (TF f2) = typeRepFingerprint (typeRep $ fromF f1)
                      == typeRepFingerprint (typeRep $ fromF f2)

fromF :: Typeable a => f a -> Proxy a
fromF _ = Proxy

calcFp :: Typeable a => f a -> Fingerprint
calcFp = typeRepFingerprint . typeRep . fromF

fromListPairs :: [(Fingerprint, Any)] -> TypeRepMap f
fromListPairs kvs = TypeRepMap (Unboxed.fromList fpAs) (Unboxed.fromList fpBs) (V.fromList ans)
  where
    (fpAs, fpBs) = unzip $ map (\(Fingerprint a b) -> (a, b)) fps
    (fps, ans) = unzip $ breadthFirst $ fromListToTree $ sortWith fst kvs

fromList :: forall f . [TF f] -> TypeRepMap f
fromList = fromListPairs . map (fp &&& an)
  where
    fp :: TF f -> Fingerprint
    fp (TF x) = calcFp x

    an :: TF f -> Any
    an (TF x) = unsafeCoerce x

----------------------------------------------------------------------------
-- Tree
----------------------------------------------------------------------------

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show)

fromListToTree :: [a] -> Tree a
fromListToTree [] = Leaf
fromListToTree xs =
    let len = length xs
    in case splitAt (len `div` 2) xs of
           ([], [])     -> Leaf
           (ls, [])     -> Node (last ls) (fromListToTree $ init ls) Leaf
           ([], (r:rs)) -> Node r Leaf $ fromListToTree rs
           (ls, r:rs)   -> Node r (fromListToTree ls) (fromListToTree rs)

breadthFirst :: Tree a -> [a]
breadthFirst tree = bf [tree]
  where
    bf :: [Tree a] -> [a]
    bf [] = []
    bf xs = mapValue xs ++ bf (concatMap children xs)

    mapValue :: [Tree a] -> [a]
    mapValue []                = []
    mapValue (Leaf : xs)       = mapValue xs
    mapValue (Node a _ _ : xs) = a : mapValue xs

    children Leaf         = []
    children (Node _ l r) = [l, r]
