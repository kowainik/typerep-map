{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.TypeRep.Map
       ( TypeRepMap (..)
       , empty
       , insert
       , lookup
       , size
       , dbgShowTree
       ) where

import Prelude hiding (lookup)

import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map.Lazy as LMap

-- | Map-like data structure with types served as the keys.
newtype TypeRepMap = TypeRepMap
    { unMap :: LMap.Map TypeRep Any
    }

-- | Empty structure.
empty :: TypeRepMap
empty = TypeRepMap mempty

-- | Inserts the value with its type as a key.
insert :: Typeable a => a -> TypeRepMap -> TypeRepMap
insert val = TypeRepMap . LMap.insert (typeOf val) (unsafeCoerce val) . unMap

-- | Looks up the value at the type.
-- >>> let x = lookup $ insert (11 :: Int) empty
-- >>> x :: Maybe Int
-- Just 11
-- >>> x :: Maybe ()
-- Nothing
lookup :: forall a . Typeable a => TypeRepMap -> Maybe a
lookup = fmap unsafeCoerce . LMap.lookup (typeRep (Proxy :: Proxy a)) . unMap

size :: TypeRepMap -> Int
size = LMap.size . unMap

dbgShowTree :: TypeRepMap -> String
dbgShowTree = LMap.showTreeWith (\k _ -> show k) False True . unMap
