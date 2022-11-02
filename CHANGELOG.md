# Changelog

`typerep-map` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.6.0.0 — Nov 2, 2022

* [#125](https://github.com/kowainik/typerep-map/issues/125):
  Support GHC-9.4.
* Allow `vector-0.13`.
* Allow `hedgehog-1.2` and `hspec-2.10`.
* Allow `criterion-1.6`.

## 0.5.0.0 — Feb 15, 2022

* [#117](https://github.com/kowainik/typerep-map/issues/117):
  Support GHC-9.2
* [#112](https://github.com/kowainik/typerep-map/issues/112):
  Change `TypeRepMap` parameter role to `representational`.

## 0.4.0.0 — Aug 3, 2021

* [#109](https://github.com/kowainik/typerep-map/issues/109):
  Support GHC-9.0.
* [#30](https://github.com/kowainik/typerep-map/issues/30):
  Remove `containers` from dependencies.
* [#94](https://github.com/kowainik/typerep-map/issues/94),
  [#99](https://github.com/kowainik/typerep-map/issues/99),
  [#100](https://github.com/kowainik/typerep-map/issues/100):
  Improve performance of `insert` and `delete`.
* [#95](https://github.com/kowainik/typerep-map/issues/95):
  Add `alter`.
* [#96](https://github.com/kowainik/typerep-map/issues/96):
  Add `intersection` and `intersectionWith`.
* [#105](https://github.com/kowainik/typerep-map/issues/105):
  Add `keysWith` and `toListWith`.

## 0.3.3.0 — Apr 18, 2020

* [#83](https://github.com/kowainik/typerep-map/issues/83):
  Support GHC-8.10.
* [#80](https://github.com/kowainik/typerep-map/issues/80):
  Build on 32-bit platforms.
* [#78](https://github.com/kowainik/typerep-map/issues/78):
  Support GHC-8.8.3.
* [#63](https://github.com/kowainik/typerep-map/pull/63):
  Add invariant check.

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
[2]: https://github.com/kowainik/typerep-map/releases
