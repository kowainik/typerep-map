import Criterion.Main

main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
