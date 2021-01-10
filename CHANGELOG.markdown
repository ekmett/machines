next [yyyy.mm.dd]
-----------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

0.7.1 [2020.10.02]
------------------
* Allow building with GHC 9.0.
* Add a `Data.Machine.MooreT` module.
* Tweak the `Corepresentable Moore` instance's implementation of `cotabulate`
  to ensure that `index . tabulate ≡ id` (note that for `Moore`,
  `tabulate = cotabulate`).

0.7 [2019.05.10]
----------------
* Remove the `Monad` instances for `Mealy` and `MealyT`, as they were
  inconsistent with the `Applicative` instances.
* Add a `Data.Machine.Group.General` module.
* Add a `takingJusts` function to `Data.Machine.Process`.
* Add `Semigroup` and `Monoid` instances for `Moore`.
* Support building with `base-4.13` (GHC 8.8).

0.6.4 [2018.07.03]
------------------
* Add `Semigroup` and `Monoid` instances for `Mealy` and `MealyT`.
* Mark `runT` and `runT_` as `INLINEABLE`.
* Increase the scope of the benchmarks. Also include the `streaming` library
  among the things that are benchmarked.
* Allow building with `containers-0.6`.

0.6.3
-----
* Add `Semigroup` instance for `Is`
* Add `MonadFail` instance for `PlanT`
* Support `doctest-0.12`

0.6.2
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.
* Various performance improvements
* Add the `flattened` and `traversing` functions, as well as the `AutomatonM`
  class, to `Data.Machine.Process`
* Add the `Data.Machine.MealyT` module
* Add `plug` to `Data.Machine.Source`
* Add `capT` to `Data.Machine.Tee`
* Fix a bug in `teeT` that caused it to run actions too many times
* Add `capWye` to `Data.Machine.Wye`

0.6.1
-----
* Bumped upper version bounds for `comonad`, `conduit-combinators`, `criterion`, `distributive`, `pointed`, and `transformers`
* Fix compilation with `stack`
* Added `strippingPrefix`, `unfold`, `unfoldT`, `zipping`

0.6
---
* Added better fanout combinators. `Data.Machine.Fanout`
* Added a module for lifting machines that run in transformed monads. `Data.Machine.Lift`
* Added instances for `Mealy` and `Moore`.
* Explicitly implemented `(<*>)` `(*>)` and `(<*)` for `PlanT`.
* Added `Data.Machine.Runner` with various tools for running machines.
* Added `teeT`.
* Added `unfoldPlan` and `preplan`

0.5.1
-----
* `profunctors` 5 support
* GHC 7.10 warnings have been cleaned up

0.5
---
* Major bug fix (and semantic change) for `Plan`'s `(<|>)`.

0.4.2
-----
* Add `Monoid` and `Semigroups` instances for `MachineT`

0.4.1
-----
* Support `void` 0.7, fixed upper bounds on dependencies going forward.

0.4.0.1
-----
* Bumped the bounds for `mtl` and `transformers`

0.4
-----

0.2.5
-----
* Added `deconstruct`, `tagDone` and `finishWith`

0.2.4
-----
* Added `asParts`, `sinkPart_`, `autoM`, and `fitM`

0.2.1
-----
* Fixed the `Mealy` Monad

0.2
---
* Removed the input type parameter from (almost) all of the types.

0.1
---
* Initial release
