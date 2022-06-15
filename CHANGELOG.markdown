## 0.5 [2022-06-15]

* `Eq` is a superclass of `Grouping`, `Ord` is a superclass of `Ordering`.
* Drop support for GHC prior 8

## 0.4.1 [2021-01-08]

* GHC-9.0 compatibility
* Added `Ordering` discrimination
* Fix `Sorting Int8` and `Sorting Int16` instances
* Fix `Integer` and `Natural` instances
* Add `NonEmpty` discrimination

## 0.4

* Added `Natural`, `Integer` and `()` discrimination

## 0.3

* Fixed a corner case where `conquer` would lie and return an empty equivalence class when fed no inputs.

## 0.2.1

* `promises` 0.3 support
* `vector` 0.11 support
* `transformers` 0.5 support
* `transformers-compat` support
* ghc 8 support

## 0.2

* `grouping` is now much more efficient.

## 0.1

* `grouping` is now productive. This means it can start spitting out results as it goes! To do this I created the `promises` package and switched to using it behind the scenes for many combinators that consume a `Group`. This has a bunch of knock-on effects:
  * `grouping` is now working properly with respect to its law!
  * `grouping` now uses an American-flag style top-down radix sort rather than a bottom up radix sort for all operations. This is sadly required for productivity. This will use a lot more memory for intermediate arrays, as we don't get to return them to storage after we're done.
  * We now use much smaller intermediate arrays for `grouping`. Should we do the same for `sorting`?

## 0

* Initialized repository
