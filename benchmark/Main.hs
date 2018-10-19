{-# LANGUAGE CPP #-}

module Main where

import Criterion.Main (defaultMain, bgroup)

import Spec
import qualified CMap
import qualified CacheMap
#if ( __GLASGOW_HASKELL__ >= 802 )
import qualified DMap
#endif
import qualified OptimalVector as OptVec

main :: IO ()
main = do
  let specs = [("CMap", CMap.spec)
              ,("CacheMap", CacheMap.spec)
#if ( __GLASGOW_HASKELL__ >= 802 )
              , ("DMap", DMap.spec)
#endif
              , ("OptVec", OptVec.spec)
              ]
      -- This code creates a benchmark group. Given a getter
      -- (that is test description) it gets a benchmark generation
      -- function from each module spec. Benchmark generation
      -- function takes a label and generate benchmarks. It's
      -- possible to introduce parameters passing in the same way.
      mkGroup getBenchmark = 
        [ mkBenchmark label
        | (label, spec) <- specs
          -- Here we use pure to force pattern matching in List
          -- then in case of pattern match failure `mzero` will
          -- be called, so benchmark will be ignored.
        , Just mkBenchmark <- pure $ getBenchmark spec
        ]
  defaultMain
    [ bgroup "lookup" $ mkGroup benchLookup 
    , bgroup "insert"
       [ bgroup "10 elements to empty" $ mkGroup benchInsertSmall
       , bgroup "1 element to big map" $ mkGroup benchInsertBig
       ]
    , bgroup "update"
       [ bgroup "10 elements to empty" $ mkGroup benchUpdateSmall
       , bgroup "1 element to big map" $ mkGroup benchUpdateBig
       ]
    ] 
