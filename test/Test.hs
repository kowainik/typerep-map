module Main
    ( main
    ) where

import Test.Hspec (hspec)

import Test.TypeRep.CMap (cMapSpec)
import Test.TypeRep.TypeRepMap (typeRepMapSpec)
import Test.TypeRep.TypeRepMapProperty (typeRepMapPropertySpec)
import Test.TypeRep.Vector (vectorSpec)
import Test.TypeRep.VectorOpt (optimalVectorSpec)


main :: IO ()
main = hspec $ do
    typeRepMapSpec
    cMapSpec
    vectorSpec
    optimalVectorSpec
    -- property
    typeRepMapPropertySpec
