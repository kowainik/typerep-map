-- | Specification of the benchmarks.
-- This module keeps a list of all bencharks, this way
-- we can group benchmark by the interesting function, not
-- by the implementation.
module Spec
  ( BenchSpec(..)
  ) where

import Criterion

-- | List of benchmarks that each module should provide.
-- If implementation can express the benchmark then it
-- can return @Nothing@ in that benchmark.
--
-- Map should contain elements from @1@ to @size of map@
-- inserted in ascending order (later that requirement may
-- change).
data BenchSpec = BenchSpec
  { benchLookup :: Maybe (String -> Benchmark)
    -- ^ Basic lookup we look 10 values inside 10k map.
    --
    -- Implementation may look like:
    -- @
    -- tenLookups :: TypeRepMap (Proxy :: Nat -> *)
    --            -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
    --               , Proxy 50, Proxy 60, Proxy 70, Proxy 80
    --               )
    -- tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
    -- @
  , benchInsertSmall :: Maybe (String -> Benchmark)
    -- ^ Insert 10 elements into an empty map.
    --
    -- Implementation may look like:
    -- @
    -- inserts :: forall a . (KnownNat a)
    --         => TypeRepMap (Proxy :: Nat -> *)
    --         -> Int
    --         -> Proxy (a :: Nat)
    --         -> TypeRepMap (Proxy :: Nat -> *)
    -- inserts !c 0 _ = c
    -- inserts !c n x = inserts (insert x c) (n-1) (Proxy :: Proxy (a+1))
    -- @
  , benchInsertBig :: Maybe (String -> Benchmark)
    -- ^ Insert 10 elements into a big map. Implementation is like
    -- a small map, but should insert values into 10k elements map.
  , benchUpdateSmall :: Maybe (String -> Benchmark)
    -- ^ Insert 10 elements into map of 10 elements, where each key
    -- was already inserted in the map
  , benchUpdateBig ::  Maybe (String -> Benchmark)
    -- ^ Insert 10 elements into map of 10k elements, where each key
    -- was already inserted in the map
  }

