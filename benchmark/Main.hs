{-# LANGUAGE CPP #-}

module Main where

import Criterion.Main (defaultMain, bgroup)

import Common
import qualified CMap
import qualified CacheMap
#if ( __GLASGOW_HASKELL__ >= 802 )
import qualified DMap
#endif
import qualified OptimalVector as OptVec

main :: IO ()
main = do
  let spec = [("CMap", CMap.spec)
             ,("CacheMap", CacheMap.spec)
#if ( __GLASGOW_HASKELL__ >= 802 )
             , ("DMap", DMap.spec)
#endif
             , ("OptVec", OptVec.spec)
             ]
      mkGroup f = 
        [ b label
        | (label, v) <- spec 
        , Just b <- pure $ f v
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
