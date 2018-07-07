{-# LANGUAGE CPP #-}

module Main where

import Criterion.Main (defaultMain)

import CacheMap (benchCacheMap, prepareBenchCacheMap)
import CMap (benchMap, prepareBenchMap)
#if ( __GLASGOW_HASKELL__ >= 802 )
import DMap (benchDMap, prepareBenchDMap)
#endif
import OptimalVector (benchVectorOpt, prepareBenchVectorOpt)
--import Vector (benchVector, prepareBenchVector)

main :: IO ()
main = do
  prepareBenchMap
  prepareBenchCacheMap
  --prepareBenchVector
  prepareBenchVectorOpt
#if ( __GLASGOW_HASKELL__ >= 802 )
  prepareBenchDMap
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
