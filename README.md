# typerep-map

[![Hackage](https://img.shields.io/hackage/v/typerep-map.svg)](https://hackage.haskell.org/package/typerep-map)
[![Build status](https://secure.travis-ci.org/kowainik/typerep-map.svg)](https://travis-ci.org/kowainik/typerep-map)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/vrom911/typerep-map/blob/master/LICENSE)

`typerep-map` introduces `TypeRepMap` — data structure like [`Map`](http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#t:Map), but where types serve as keys, and values have the types specified in the corresponding key spots.

## Usage example

```haskell
ghci> let typeRepMap = insert (Identity True) $ one (Identity (42 :: Int))

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

ghci> member @Int typeRepMap
True

ghci> let trMap = delete @Int typeRepMap

ghci> member @Int trMap
False
```

## Benchmarks

Tables below contain comparision with `DMap TypeRep` of `lookup` operation
on structure with size `10^4`:

|                | ghc-8.2.2 | ghc-8.4.3 |
|----------------|-----------|-----------|
| `DMap TypeRep` | 517.5 ns  | 779.9 ns  |
| `typerep-map`  | 205.3 ns  | 187.2 ns  |

 ghc-8.2.2 |  ghc-8.4.3
:---------:|:-----------:
![DMap 8.2.2](https://user-images.githubusercontent.com/4276606/42495129-c700f21e-8454-11e8-98b4-ba080259c712.png) | ![DMap 8.4.3](https://user-images.githubusercontent.com/4276606/42495168-ebb1d13c-8454-11e8-9d17-f6da29d2169a.png)
![TMap 8.2.2](https://user-images.githubusercontent.com/4276606/42494935-3a352d96-8454-11e8-985e-ebc77cc51ca0.png) | ![TMap 8.4.3](https://user-images.githubusercontent.com/4276606/42495147-d884bdf4-8454-11e8-887f-9815fd2b8d68.png)
