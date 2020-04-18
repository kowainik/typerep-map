{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}

{- |
Copyright:  (c) 2017-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

'TMap' is a heterogeneous data structure similar in its essence to
'Data.Map.Map' with types as keys, where each value has the type of its key.

Here is an example of a 'TMap' with a comparison to 'Data.Map.Map':

@
 'Data.Map.Map' 'Prelude.String' 'Prelude.String'             'TMap'
--------------------     -----------------
 \"Int\"  -> \"5\"             'Prelude.Int'  -> 5
 \"Bool\" -> \"True\"          'Prelude.Bool' -> 'Prelude.True'
 \"Char\" -> \"\'x\'\"           'Prelude.Char' -> \'x\'
@

The runtime representation of 'TMap' is an array, not a tree. This makes
'lookup' significantly more efficient.

-}

module Data.TMap
       ( -- * Map type
         TMap

         -- * Construction
       , empty
       , one

         -- * Modification
       , insert
       , delete
       , unionWith
       , union
       , map
       , adjust

         -- * Query
       , lookup
       , member
       , size
       , keys
       ) where

import Prelude hiding (lookup, map)

import Data.Functor.Identity (Identity (..))
import Data.Typeable (Typeable)
import GHC.Exts (coerce)
import Type.Reflection (SomeTypeRep)

import qualified Data.TypeRepMap as F

-- | 'TMap' is a special case of 'F.TypeRepMap' when the interpretation is
-- 'Identity'.
type TMap = F.TypeRepMap Identity

{- |

A 'TMap' with no values stored in it.

prop> size empty == 0
prop> member @a empty == False

-}
empty :: TMap
empty = F.empty
{-# INLINE empty #-}

{- |

Construct a 'TMap' with a single element.

prop> size (one x) == 1
prop> member @a (one (x :: a)) == True

-}
one :: forall a . Typeable a => a -> TMap
one x = coerce (F.one @a @Identity $ coerce x)
{-# INLINE one #-}

{- |

Insert a value into a 'TMap'.

prop> size (insert v tm) >= size tm
prop> member @a (insert (x :: a) tm) == True

-}
insert :: forall a . Typeable a => a -> TMap -> TMap
insert x = coerce (F.insert @a @Identity $ coerce x)
{-# INLINE insert #-}

{- | Delete a value from a 'TMap'.

prop> size (delete @a tm) <= size tm
prop> member @a (delete @a tm) == False

>>> tm = delete @Bool $ insert True $ one 'a'
>>> size tm
1
>>> member @Bool tm
False
>>> member @Char tm
True
-}
delete :: forall a . Typeable a => TMap -> TMap
delete = F.delete @a @Identity
{-# INLINE delete #-}

-- | The union of two 'TMap's using a combining function.
unionWith :: (forall x. Typeable x => x -> x -> x) -> TMap -> TMap -> TMap
unionWith f = F.unionWith fId
  where
    fId :: forall y . Typeable y => Identity y -> Identity y -> Identity y
    fId y1 y2 = Identity $ f (coerce y1) (coerce y2)
{-# INLINE unionWith #-}

-- | The (left-biased) union of two 'TMap's. It prefers the first map when
-- duplicate keys are encountered, i.e. @'union' == 'unionWith' const@.
union :: TMap -> TMap -> TMap
union = F.union
{-# INLINE union #-}

{- | Lookup a value of the given type in a 'TMap'.

>>> x = lookup $ insert (11 :: Int) empty
>>> x :: Maybe Int
Just 11
>>> x :: Maybe ()
Nothing
-}
lookup :: forall a. Typeable a => TMap -> Maybe a
lookup = coerce (F.lookup @a @Identity)
{-# INLINE lookup #-}

{- | Check if a value of the given type is present in a 'TMap'.

>>> member @Char $ one 'a'
True
>>> member @Bool $ one 'a'
False
-}
member :: forall a . Typeable a => TMap -> Bool
member = F.member @a @Identity
{-# INLINE member #-}

-- | Get the amount of elements in a 'TMap'.
size :: TMap -> Int
size = F.size
{-# INLINE size #-}

-- | Returns the list of 'SomeTypeRep's from keys.
keys :: TMap -> [SomeTypeRep]
keys = F.keys
{-# INLINE keys #-}

-- | Map a function over the values.
map :: (forall a. Typeable a => a -> a) -> TMap -> TMap
map f = F.hoistWithKey (liftToIdentity f)
{-# INLINE map #-}

-- | Update a value with the result of the provided function.
adjust :: Typeable a => (a -> a) -> TMap -> TMap
adjust f = F.adjust (liftToIdentity f)
{-# INLINE adjust #-}

liftToIdentity :: forall a. (a -> a) -> Identity a -> Identity a
liftToIdentity = coerce
