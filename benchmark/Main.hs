{-# LANGUAGE CPP #-}

module Main where

import Criterion.Main (defaultMain)

import CacheMap (benchCacheMap)
import CMap (benchMap)
#if ( __GLASGOW_HASKELL__ >= 802 )
import DMap (benchDMap)
#endif
import OptimalVector (benchVectorOpt)

main :: IO ()
main = do
#if ( __GLASGOW_HASKELL__ >= 802 )
#endif
  defaultMain
    [ benchMap
   -- , benchVector
    , benchCacheMap
    , benchVectorOpt
#if ( __GLASGOW_HASKELL__ >= 802 )
    , benchDMap
#endif
    ]
