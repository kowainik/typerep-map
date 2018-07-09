# typerep-map

[![Hackage](https://img.shields.io/hackage/v/typerep-map.svg)](https://hackage.haskell.org/package/typerep-map)
[![Build status](https://secure.travis-ci.org/kowainik/typerep-map.svg)](https://travis-ci.org/kowainik/typerep-map)
[![Stackage LTS](http://stackage.org/package/typerep-map/badge/lts)](http://stackage.org/lts/package/typerep-map)
[![Stackage Nightly](http://stackage.org/package/typerep-map/badge/nightly)](http://stackage.org/nightly/package/typerep-map)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/vrom911/typerep-map/blob/master/LICENSE)

`typerep-map` introduces `TypeRepMap` — data structure like [`Map`](http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#t:Map), but where types serve as keys, and values have the types specified in the corresponding key spots.

```haskell
ghci> let typeRepMap = insert (Identity True) $ insert (Identity (42 :: Int)) empty

ghci> size typeRepMap
2

ghci> let res = lookup typeRepMap

ghci> res :: Maybe (Identity Int)
Just (Identity 42)

ghci> res :: Maybe (Identity Bool)
Just (Identity True)

ghci> res :: Maybe (Identity String)
Nothing

ghci> lookup (insert (Identity "hello") typeRepMap) :: Maybe (Identity String)
Just (Identity "hello")
```
