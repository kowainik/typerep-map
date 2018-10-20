# typerep-map

![electricity](https://user-images.githubusercontent.com/8126674/44323413-788dd700-a484-11e8-842e-f224cfaa4206.png)
[![Hackage](https://img.shields.io/hackage/v/typerep-map.svg)](https://hackage.haskell.org/package/typerep-map)
[![Build status](https://secure.travis-ci.org/kowainik/typerep-map.svg)](https://travis-ci.org/kowainik/typerep-map)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/vrom911/typerep-map/blob/master/LICENSE)

`typerep-map` introduces `TMap` and `TypeRepMap` — data structures like [`Map`](http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#t:Map), but where types serve as keys, and values have the types specified in the corresponding key spots.

For the more details on the implementation see [this blog post](https://kowainik.github.io/posts/2018-07-11-typerep-map-step-by-step).

## Usage example

```haskell
ghci> import Data.TMap

ghci> tm = insert True $ one (42 :: Int)

ghci> size tm
2

ghci> res = lookup tm

ghci> res :: Maybe Int
Just 42

ghci> res :: Maybe Bool
Just True

ghci> res :: Maybe String
Nothing

ghci> lookup (insert "hello" tm) :: Maybe String
Just "hello"

ghci> member @Int tm
True

ghci> tm' = delete @Int tm

ghci> member @Int tm'
False
```

## Benchmarks

Tables below contain comparision with `DMap TypeRep` of ten `lookup` operations
on structure with size `10^4`:

|                | ghc-8.2.2 | ghc-8.4.3 |
|----------------|-----------|-----------|
| `DMap TypeRep` | 517.5 ns  | 779.9 ns  |
| `typerep-map`  | 205.3 ns  | 187.2 ns  |

 ghc-8.2.2 |  ghc-8.4.3
:---------:|:-----------:
![DMap 8.2.2](https://user-images.githubusercontent.com/4276606/42495129-c700f21e-8454-11e8-98b4-ba080259c712.png) | ![DMap 8.4.3](https://user-images.githubusercontent.com/4276606/42495168-ebb1d13c-8454-11e8-9d17-f6da29d2169a.png)
![TMap 8.2.2](https://user-images.githubusercontent.com/4276606/42494935-3a352d96-8454-11e8-985e-ebc77cc51ca0.png) | ![TMap 8.4.3](https://user-images.githubusercontent.com/4276606/42495147-d884bdf4-8454-11e8-887f-9815fd2b8d68.png)
