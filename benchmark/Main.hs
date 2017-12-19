module Main where

import Criterion.Main (defaultMain)

import Map (benchMap, prepareBenchMap)
import OptimalVector (benchVectorOpt, prepareBenchVectorOpt)
--import Vector (benchVector, prepareBenchVector)

main :: IO ()
main = do
  prepareBenchMap
  --prepareBenchVector
  prepareBenchVectorOpt
  defaultMain
    [ benchMap
   -- , benchVector
    , benchVectorOpt
    ]
