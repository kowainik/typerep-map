module Main where

import Criterion.Main (defaultMain)

import CacheMap (benchCacheMap, prepareBenchCacheMap)
import Map (benchMap, prepareBenchMap)
import OptimalVector (benchVectorOpt, prepareBenchVectorOpt)
--import Vector (benchVector, prepareBenchVector)

main :: IO ()
main = do
  prepareBenchMap
  prepareBenchCacheMap
  --prepareBenchVector
  prepareBenchVectorOpt
  defaultMain
    [ benchMap
   -- , benchVector
    , benchCacheMap
    , benchVectorOpt
    ]
