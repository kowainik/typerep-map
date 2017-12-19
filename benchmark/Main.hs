module Main where

import Criterion.Main (defaultMain)

import Map (benchMap)
import OptimalVector (benchVectorOpt)
--import Vector (benchVector)

main :: IO ()
main = defaultMain
    [ benchMap
    --, benchVector
    , benchVectorOpt
    ]
