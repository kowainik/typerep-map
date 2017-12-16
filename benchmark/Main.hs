{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main (bench, defaultMain, whnf)

import Prelude hiding (lookup)

import Control.DeepSeq (rnf)
import Control.Exception.Base (evaluate)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)

import Data.TypeRep.Map (TypeRepMap (..), empty, insert, keys, lookup, size)

main :: IO ()
main = do
    putStrLn $ "size: " ++ (show $ size bigMap)
    evaluate $ rknf bigMap
    defaultMain
        [ bench "lookup"     $ whnf (lookup :: TypeRepMap -> Maybe Char) bigMap
        , bench "insert new" $ whnf (\x -> insert x bigMap) (111 :: Int)
        , bench "update old" $ whnf (\x -> insert x bigMap) 'b'
        ]

-- TypeRepMap of 10000 elements
bigMap :: TypeRepMap
bigMap = buildBigMap 10000 (Proxy :: Proxy Z) empty

data Z
data S a

buildBigMap :: forall a . Typeable a => Int -> Proxy a -> TypeRepMap -> TypeRepMap
buildBigMap 1 x = insert 'a' . insert x
buildBigMap n x = insert x . buildBigMap (n - 1) (Proxy :: Proxy (S a))

rknf :: TypeRepMap -> ()
rknf = rnf . keys
