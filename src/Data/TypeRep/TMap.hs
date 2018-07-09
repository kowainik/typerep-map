{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}

-- | This module introduces 'TMap' data type — 'TR.TypeRepMap' parametrized by 'Identity'.

module Data.TypeRep.TMap
       ( -- * Map type
         TMap

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
       ) where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Data.Typeable (Typeable)
import GHC.Exts (coerce)

import qualified Data.TypeRep.Map as TR

-- | 'TR.TypeRepMap' parametrized by 'Identity'.
type TMap = TR.TypeRepMap Identity

-- | Similar to 'TR.empty' but for 'TMap'.
empty :: TMap
empty = TR.empty
{-# INLINE empty #-}

-- | Similar to 'TR.one' but for 'TMap'.
one :: forall a . Typeable a => a -> TMap
one x = coerce (TR.one @a @Identity $ coerce x)
{-# INLINE one #-}

-- | Similar to 'TR.insert' but for 'TMap'.
insert :: forall a . Typeable a => a -> TMap -> TMap
insert x = coerce (TR.insert @a @Identity $ coerce x)
{-# INLINE insert #-}

-- | Similar to 'TR.delete' but for 'TMap'.
delete :: forall a . Typeable a => TMap -> TMap
delete = TR.delete @a @Identity
{-# INLINE delete #-}

-- | Similar to 'TR.hoist' but for 'TMap'.
hoist :: (forall x. x -> g x) -> TMap -> TR.TypeRepMap g
hoist f = TR.hoist (f . coerce)
{-# INLINE hoist #-}

-- | Similar to 'TR.unionWith' but for 'TMap'.
unionWith :: (forall x. x -> x -> x) -> TMap -> TMap -> TMap
unionWith f = TR.unionWith fId
  where
    fId :: forall y . Identity y -> Identity y -> Identity y
    fId y1 y2 = Identity $ f (coerce y1) (coerce y2)
{-# INLINE unionWith #-}

-- | Similar to 'TR.union' but for 'TMap'.
union :: TMap -> TMap -> TMap
union = TR.union
{-# INLINE union #-}

-- | Similar to 'TR.lookup' but for 'TMap'.
lookup :: forall a. Typeable a => TMap -> Maybe a
lookup = coerce (TR.lookup @a @Identity)
{-# INLINE lookup #-}

-- | Similar to 'TR.member' but for 'TMap'.
member :: forall a . Typeable a => TMap -> Bool
member = TR.member @a @Identity
{-# INLINE member #-}

-- | Similar to 'TR.size' but for 'TMap'.
size :: TMap -> Int
size = TR.size
{-# INLINE size #-}
