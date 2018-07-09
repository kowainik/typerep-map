-- | This module introduces the 'TypeRepMap' data structure and functions to work with it.

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
