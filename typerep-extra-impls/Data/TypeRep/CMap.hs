{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TypeRepMap implementation based on @containers@ 'Map'.
-}

module Data.TypeRep.CMap
       ( TypeRepMap (..)
       , empty
       , insert
       , keys
       , lookup
       , size
       ) where

import Prelude hiding (lookup)

import Control.DeepSeq
import Data.Kind (Type)
import Type.Reflection (TypeRep, Typeable, typeRep, SomeTypeRep (..), withTypeable)

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))


-- | Map-like data structure with types served as the keys.
newtype TypeRepMap (f :: k -> Type) = TypeRepMap
    { unMap :: DMap.DMap TypeRep f
    }

#if __GLASGOW_HASKELL__ >= 806
instance forall k (f :: k -> Type). (forall (v :: k). Typeable v => NFData (f v)) => NFData (TypeRepMap f) where
  rnf = DMap.foldrWithKey (\ kv fv r -> withTypeable kv $ fv `deepseq` r) () . unMap
#endif

-- | Empty structure.
empty :: TypeRepMap f
empty = TypeRepMap DMap.empty

-- | Inserts the value with its type as a key.
insert :: forall a f . Typeable a => f a -> TypeRepMap f -> TypeRepMap f
insert val = TypeRepMap . DMap.insert (typeRep @a) val . unMap

-- | Looks up the value at the type.
-- >>> let x = lookup $ insert (11 :: Int) empty
-- >>> x :: Maybe Int
-- Just 11
-- >>> x :: Maybe ()
-- Nothing
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
lookup = DMap.lookup (typeRep @a) . unMap

size :: TypeRepMap f -> Int
size = DMap.size . unMap

keys :: TypeRepMap f -> [SomeTypeRep]
keys (TypeRepMap m) = [SomeTypeRep k | k :=> _ <- DMap.assocs m]
