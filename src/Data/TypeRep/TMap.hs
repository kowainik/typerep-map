{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module introduces 'TMap' data type — 'TR.TypeRepMap' parametrized by 'Identity'.

module Data.TypeRep.TMap
       ( -- * Map type
         TMap

         -- * Construction
       , TR.empty
       , one

         -- * Modification
       , insert
       , delete
       , TR.hoist
       , TR.unionWith
       , TR.union

         -- * Query
       , lookup
       , member
       , TR.size
       ) where

import Prelude hiding (lookup)

import Data.Functor.Identity (Identity (..))
import Data.Typeable (Typeable)
import GHC.Exts (coerce)

import qualified Data.TypeRep.Map as TR

-- | 'TR.TypeRepMap' parametrized by 'Identity'.
type TMap = TR.TypeRepMap Identity

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
delete = coerce (TR.delete @a @Identity)
{-# INLINE delete #-}

-- | Similar to 'TR.lookup' but for 'TMap'.
lookup :: forall a. Typeable a => TMap -> Maybe a
lookup = coerce (TR.lookup @a @Identity)
{-# INLINE lookup #-}

-- | Similar to 'TR.member' but for 'TMap'.
member :: forall a . Typeable a => TMap -> Bool
member = coerce (TR.member @a @Identity)
