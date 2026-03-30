0.4.0.0

* Add `ToJSON` and `FromJSON` instances for `Natural`, `Positive`, and all newtype wrappers (`ProductNatural`, `MaxNatural`, `MinNatural`, `SumPositive`, `MaxPositive`, `MinPositive`).
* Add `toJsonNatural`, `parseJsonNatural`, `toJsonPositive`, `parseJsonPositive` functions.
* Add `aeson` dependency.
* Rename `square` to `power` and `square1` to `power1`.
* Fix `(!!)` which previously returned `Nothing` for all inputs.
* Fix `(!!!)` which previously returned `Nothing` for all inputs.
* Fix `multiply1` which was performing addition instead of multiplication.
* Fix `minus1` off-by-one when arguments are equal.
* Remove invalid `Monoid Positive` instance (`mempty` was `Positive 0`).
* Add Haddock documentation and doctests.

0.3.0.7

* Update version bounds

0.3.0.6

* Support `lens` up to `5.1`

0.3.0.5

* Support GHC 8.10.1.

0.3.0.4

* add `naturalPositive` iso and `instance AsPositive Natural` prism.

0.3.0.3

* added some more functions.

0.3.0.2

* fix bug in `zero` and `one` prisms.
* rename `Natural` to `Positive`.
* rename `Whole` to `Natural`.

0.2.0.2

* change `data Natural` to have a minimum bound of 1.
* add `data Whole` which has a minimum bound of 0.

0.1.0.2

* add `minus` function.
* add `one` and `one'` values.
* add `list` iso.

0.1.0.1

* Add support for GHC 7.10 and GHC 8.4.

0.1.0.0

* This change log starts.
* The initial version of natural.
