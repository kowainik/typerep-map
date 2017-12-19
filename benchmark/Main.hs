module Main where

import Criterion.Main (defaultMain)

import Map (benchMap, prepareBenchMap)
-- import OptimalVector (benchVectorOpt)
import Vector (benchVector, prepareBenchVector)

main :: IO ()
main = do
  prepareBenchMap
  prepareBenchVector
  defaultMain
    [ benchMap
    , benchVector
--    , benchVectorOpt
    ]
