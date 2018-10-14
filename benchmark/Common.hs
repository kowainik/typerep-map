module Common
  ( BenchSpec(..)
  ) where

import Criterion

data BenchSpec = BenchSpec
  { benchLookup :: Maybe (String -> Benchmark)
  , benchInsertSmall :: Maybe (String -> Benchmark)
  , benchInsertBig :: Maybe (String -> Benchmark)
  , benchUpdateSmall :: Maybe (String -> Benchmark)
  , benchUpdateBig ::  Maybe (String -> Benchmark)
  }

