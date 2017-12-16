{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import Criterion.Main (bench, defaultMain, nf, whnf)

import Prelude hiding (lookup)

import Control.DeepSeq (rnf)
import Control.Exception.Base (evaluate)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.TypeLits (type (-), Nat)

import Data.TypeRep.Map (TypeRepMap (..), empty, insert, keys, lookup, size)

main :: IO ()
main = do
    putStrLn $ "size: " ++ (show $ size bigMap)
    evaluate $ rknf bigMap
    defaultMain
        [ bench "lookup"     $ nf tenLookups bigMap
        , bench "insert new" $ whnf (\x -> rknf $ insert x bigMap) (111 :: Int)
        , bench "update old" $ whnf (\x -> rknf $ insert x bigMap) 'b'
        ]

tenLookups :: TypeRepMap
           -> ( Proxy (BigProxy 10), Proxy (BigProxy 20)
              , Proxy (BigProxy 30), Proxy (BigProxy 40)
              , Proxy (BigProxy 50), Proxy (BigProxy 60)
              , Proxy (BigProxy 70), Proxy (BigProxy 80)
              -- , Proxy (BigProxy 9000), Proxy (BigProxy 10000)
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: Typeable a => Proxy a
    lp = fromJust $ lookup tmap

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

type family BigProxy (n :: Nat) :: * where
    BigProxy 0 = Z
    BigProxy n = S (BigProxy (n - 1))
