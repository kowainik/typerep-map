module Data.TypeRep.Map
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
       ) where

import Prelude hiding (lookup)

import Data.TypeRep.CacheMap
