# natural

A Haskell library providing type-safe natural numbers (`Natural`, non-negative) and positive integers (`Positive`, strictly positive), built around [`lens`](https://hackage.haskell.org/package/lens) optics rather than the standard Haskell numeric type-class hierarchy.

![System-F](https://logo.systemf.com.au/systemf-450x450.png)

## Overview

The library exposes two core types:

* **`Natural`** — non-negative integers (0, 1, 2, ...) with arithmetic (`plus`, `multiply`, `square`, `minus`), monoid wrappers (`ProductNatural`, `MaxNatural`, `MinNatural`), and safe list operations (`length`, `replicate`, `take`, `drop`, `splitAt`, `!!`, index search).

* **`Positive`** — strictly positive integers (1, 2, 3, ...) with corresponding arithmetic (`plus1`, `multiply1`, `square1`, `minus1`), monoid wrappers (`SumPositive`, `MaxPositive`, `MinPositive`), and `NonEmpty` list operations (`length1`, `replicate1`, `take1`, `drop1`, `splitAt1`, `!!!`, index search).

Conversions between the two types are provided via prisms and isomorphisms (`notZero`, `naturalPositive`, `successorW`, `plusone`, `minusone`).

## Design

* **Prism-based conversion** — `AsNatural` and `AsPositive` provide prisms from integral types (`Int`, `Integer`, `Word`, etc.), making invalid values unrepresentable rather than throwing runtime exceptions.
* **Lens integration** — `HasNatural` and `HasPositive` provide lenses for composable access and update.
* **Structural isomorphisms** — `Natural` is isomorphic to `[()]` and `Positive` to `NonEmpty ()`, connecting numeric and list representations.

## FAQ

* **Why not use [`Numeric.Natural`](http://hackage.haskell.org/package/base/docs/Numeric-Natural.html) from `base`?**

  The `base` type integrates with Haskell's numeric type-classes at the cost of safety — partial functions like `fromInteger (-1)` produce runtime errors. This library avoids that by using `lens` optics for total, composable conversions.

