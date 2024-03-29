# typerep-map

![logo](https://user-images.githubusercontent.com/8126674/44323413-788dd700-a484-11e8-842e-f224cfaa4206.png)

[![GitHub CI](https://github.com/kowainik/typerep-map/workflows/CI/badge.svg)](https://github.com/kowainik/typerep-map/actions)
[![Hackage](https://img.shields.io/hackage/v/typerep-map.svg?logo=haskell)](https://hackage.haskell.org/package/typerep-map)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)


`typerep-map` introduces `TMap` and `TypeRepMap` — data structures like [`Map`](http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#t:Map), but where types serve as keys, and values have the types specified in the corresponding key spots.

For the more details on the implementation see the following blog post:

* [typerep-map step by step](https://kowainik.github.io/posts/2018-07-11-typerep-map-step-by-step)

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

|                | ghc-8.2.2 | ghc-8.4.3 | ghc-8.8.3 | ghc-8.10.1 |
|----------------|-----------|-----------|-----------|------------|
| `DMap TypeRep` | 517.5 ns  | 779.9 ns  | 1.559 μs  | 1.786 μs   |
| `typerep-map`  | 205.3 ns  | 187.2 ns  | 190.1 ns  | 169.1 ns   |

 ghc-8.2.2 |  ghc-8.4.3
:---------:|:-----------:
![DMap 8.2.2](https://user-images.githubusercontent.com/4276606/42495129-c700f21e-8454-11e8-98b4-ba080259c712.png) | ![DMap 8.4.3](https://user-images.githubusercontent.com/4276606/42495168-ebb1d13c-8454-11e8-9d17-f6da29d2169a.png)
![TMap 8.2.2](https://user-images.githubusercontent.com/4276606/42494935-3a352d96-8454-11e8-985e-ebc77cc51ca0.png) | ![TMap 8.4.3](https://user-images.githubusercontent.com/4276606/42495147-d884bdf4-8454-11e8-887f-9815fd2b8d68.png)
