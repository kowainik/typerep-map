# Changelog

`typerep-map` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.3.2 — Mar 27, 2019

* [#47](https://github.com/kowainik/typerep-map/issues/47):
  Add `Eq` instance for `TypeRepMap` using `-XQuantifiedConstraints`.
* [#70](https://github.com/kowainik/typerep-map/issues/70):
  Bump up to `dependent-sum-0.5`.

## 0.3.1

* [#64](https://github.com/kowainik/typerep-map/issues/64):
  Fix segfault in `toList`.
* Support GHC 8.4.4 and 8.6.3.

## 0.3.0

* [#46](https://github.com/kowainik/typerep-map/issues/46):
  Make `Show` instance for `TypeRepMap` show keys.
  Add `keys` function.
* [#48](https://github.com/kowainik/typerep-map/issues/48):
  Add `adjust` function for `TypeRepMap` and  `TMap`.
* [#30](https://github.com/kowainik/typerep-map/issues/30):
  Rewrite `fromSortedList` to use `Array` and `MutableArray`
  instead of `IntMap`.


## 0.2.0

* [#43](https://github.com/kowainik/typerep-map/issues/43):
  Implement `IsList` instance for `TypeRepMap`.
  Add `hoistA` function.
* [#39](https://github.com/kowainik/typerep-map/issues/39):
  Implement `hoistWithKey` function.
  Add `map` function for `TMap`.
* Drop support for `ghc-8.0.2`.

## 0.1.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/typerep-map/blob/master/CHANGELOG.md
