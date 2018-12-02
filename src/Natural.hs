{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Natural (
  Natural
, HasNatural(..)
, AsNatural(..)
, ProductNatural(..)
, MaxNatural(..)
, MinNatural(..)
, zero
, zero'
, successor
, successor'
, plus
, multiply
, square
, zeroOr
, length
, replicate
, take
, drop
, splitAt
, (!!)
, findIndices
, findIndex
, elemIndices
, elemIndex
, minus
, list
, Positive
, HasPositive(..)
, AsPositive(..)
, SumPositive(..)
, MaxPositive(..)
, MinPositive(..)
, one
, one'
, successor1
, successor1'
, successorW
, plus1
, multiply1
, square1
, oneOr
, notZero
, length1
, replicate1
, take1
, drop1
, splitAt1
, (!!!)
, findIndices1
, findIndex1
, elemIndices1
, elemIndex1
, minus1
, list1
, plusone
, minusone
) where

import Control.Applicative(Const)
import Control.Category((.), id)
import Control.Lens(Wrapped(_Wrapped', Unwrapped), Rewrapped, Prism', Lens', Iso', (^?), ( # ), (^.), _Wrapped, prism', iso)
import Control.Monad((>>=))
import Data.Bool(Bool)
import Data.Eq(Eq((==)))
import Data.Foldable(Foldable(foldl))
import Data.Function(const)
import Data.Functor.Identity(Identity)
import Data.Int(Int)
import Data.List(iterate, zip, filter, map, repeat)
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty(iterate, zip, filter)
import Data.Maybe(listToMaybe, Maybe(Just, Nothing), fromMaybe)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord((<)), min, max)
import Data.Semigroup(Semigroup((<>)))
import Data.Semigroup.Foldable(Foldable1(foldMap1))
import Data.Tuple(fst, snd)
import Data.Word(Word)
import Prelude(Show, Integral, Integer, (-), (+), (*), (^), fromIntegral)

newtype Natural =
  Natural
    Integer
  deriving (Eq, Ord, Show)

instance Semigroup Natural where
  Natural x <> Natural y =
    Natural (x + y)

instance Monoid Natural where
  mappend =
    (<>)
  mempty =
    Natural 0

class HasNatural a where
  natural ::
    Lens'
      a
      Natural

instance HasNatural Natural where
  natural =
    id

class AsNatural a where
  _Natural ::
    Prism'
      a
      Natural

instance AsNatural Natural where
  _Natural =
    id

integralPrism ::
  Integral a =>
  Prism'
    a
    Natural
integralPrism =
  prism'
    (\(Natural n) -> fromIntegral n)
    (\n -> if n < 0 then Nothing else Just (Natural (fromIntegral n)))

instance AsNatural Int where
  _Natural =
    integralPrism

instance AsNatural Integer where
  _Natural =
    integralPrism

instance AsNatural Word where
  _Natural =
    integralPrism

instance Integral a => AsNatural (Const a b) where
  _Natural =
    integralPrism

instance Integral a => AsNatural (Identity a) where
  _Natural =
    integralPrism

newtype ProductNatural =
  ProductNatural
    Natural
  deriving (Eq, Ord, Show)

instance HasNatural ProductNatural where
  natural =
    _Wrapped . natural

instance AsNatural ProductNatural where
  _Natural =
    _Wrapped . _Natural

instance ProductNatural ~ a =>
  Rewrapped ProductNatural a
  
instance Wrapped ProductNatural where
  type Unwrapped ProductNatural = Natural
  _Wrapped' =
    iso
      (\(ProductNatural x) -> x)
      ProductNatural

instance Semigroup ProductNatural where
  ProductNatural (Natural x) <> ProductNatural (Natural y) =
    ProductNatural (Natural (x * y))

instance Monoid ProductNatural where
  mappend =
    (<>)
  mempty =
    ProductNatural (Natural 1)

newtype MaxNatural =
  MaxNatural
    Natural
  deriving (Eq, Ord, Show)

instance HasNatural MaxNatural where
  natural =
    _Wrapped . natural

instance AsNatural MaxNatural where
  _Natural =
    _Wrapped . _Natural

instance MaxNatural ~ a =>
  Rewrapped MaxNatural a
  
instance Wrapped MaxNatural where
  type Unwrapped MaxNatural = Natural
  _Wrapped' =
    iso
      (\(MaxNatural x) -> x)
      MaxNatural

instance Semigroup MaxNatural where
  MaxNatural (Natural x) <> MaxNatural (Natural y) =
    MaxNatural (Natural (x `max` y))

newtype MinNatural =
  MinNatural
    Natural
  deriving (Eq, Ord, Show)

instance HasNatural MinNatural where
  natural =
    _Wrapped . natural

instance AsNatural MinNatural where
  _Natural =
    _Wrapped . _Natural

instance MinNatural ~ a =>
  Rewrapped MinNatural a
  
instance Wrapped MinNatural where
  type Unwrapped MinNatural = Natural
  _Wrapped' =
    iso
      (\(MinNatural x) -> x)
      MinNatural

instance Semigroup MinNatural where
  MinNatural (Natural x) <> MinNatural (Natural y) =
    MinNatural (Natural (x `min` y))

zero ::
  Prism'
    Natural
    ()
zero =
  prism'
    (\() -> Natural 0)
    (\(Natural n) -> if n == 0 then Just () else Nothing)

zero' ::
  Natural
zero' =
  zero # ()

successor ::
  Prism'
    Natural
    Natural
successor =
  prism'
    (\(Natural n) -> Natural (n + 1))
    (\(Natural n) -> if n == 0 then Nothing else Just (Natural (n - 1)))

successor' ::
  Natural
  -> Natural
successor' =
  (successor #)

plus ::
  Natural
  -> Natural
  -> Natural
plus =
  (<>)

multiply ::
  Natural
  -> Natural
  -> Natural
multiply x y =
  (_Wrapped # x <> (_Wrapped # y :: ProductNatural)) ^. _Wrapped

square ::
  Natural
  -> Natural
  -> Natural
square (Natural x) (Natural y) =
  Natural (x ^ y)

zeroOr ::
  AsNatural a =>
  a
  -> Natural
zeroOr n =
  fromMaybe zero' (n ^? _Natural)

length ::
  Foldable f =>
  f a
  -> Natural
length =
  foldl (const . successor') zero'

replicate ::
  Natural
  -> a
  -> [a]
replicate n =
  take n . repeat

take ::
  Natural
  -> [a]
  -> [a]
take _ [] =
  []
take n (h:t) =
  case n ^? successor of
    Nothing ->
      []
    Just p ->
      h : take p t

drop ::
  Natural
  -> [a]
  -> [a]
drop _ [] =
  []
drop n (h:t) =
  case n ^? successor of
    Nothing ->
      h:t
    Just p ->
      drop p t

splitAt ::
  Natural
  -> [a]
  -> ([a], [a])
splitAt n x =
  (take n x, drop n x)

(!!) ::
  [a]
  -> Natural
  -> Maybe a
[] !! _ =
  Nothing
(_:t) !! n =
  (n ^? successor) >>= (t !!)

findIndices ::
  (a -> Bool)
  -> [a]
  -> [Natural]
findIndices p x =
  map snd (filter (p . fst) (zip x (iterate successor' zero')))

findIndex ::
  (a -> Bool)
  -> [a]
  -> Maybe Natural
findIndex p =
  listToMaybe . findIndices p

elemIndices ::
  Eq a =>
  a
  -> [a]
  -> [Natural]
elemIndices =
  findIndices . (==)

elemIndex ::
  Eq a =>
  a
  -> [a]
  -> Maybe Natural
elemIndex =
  findIndex . (==)

minus ::
  Natural
  -> Natural
  -> Natural
minus (Natural x) (Natural y) =
  Natural (if x < y then 0 else x - y)

list ::
  Iso'
    Natural
    [()]
list =
  iso
    (\n -> replicate n ())
    length

----

newtype Positive =
  Positive
    Integer
  deriving (Eq, Ord, Show)

instance Semigroup Positive where
  Positive x <> Positive y =
    Positive (x + y)

instance Monoid Positive where
  mappend =
    (<>)
  mempty =
    Positive 0

class HasPositive a where
  positive ::
    Lens'
      a
      Positive

instance HasPositive Positive where
  positive =
    id

class AsPositive a where
  _Positive ::
    Prism'
      a
      Positive

instance AsPositive Positive where
  _Positive =
    id

integralPrism1 ::
  Integral a =>
  Prism'
    a
    Positive
integralPrism1 =
  prism'
    (\(Positive n) -> fromIntegral n)
    (\n -> if n < 1 then Nothing else Just (Positive (fromIntegral n)))

instance AsPositive Int where
  _Positive =
    integralPrism1

instance AsPositive Integer where
  _Positive =
    integralPrism1

instance AsPositive Word where
  _Positive =
    integralPrism1

instance Integral a => AsPositive (Const a b) where
  _Positive =
    integralPrism1

instance Integral a => AsPositive (Identity a) where
  _Positive =
    integralPrism1

newtype SumPositive =
  SumPositive
    Positive
  deriving (Eq, Ord, Show)

instance HasPositive SumPositive where
  positive =
    _Wrapped . positive

instance AsPositive SumPositive where
  _Positive =
    _Wrapped . _Positive

instance SumPositive ~ a =>
  Rewrapped SumPositive a
  
instance Wrapped SumPositive where
  type Unwrapped SumPositive = Positive
  _Wrapped' =
    iso
      (\(SumPositive x) -> x)
      SumPositive

instance Semigroup SumPositive where
  SumPositive (Positive x) <> SumPositive (Positive y) =
    SumPositive (Positive (x + y))

newtype MaxPositive =
  MaxPositive
    Positive
  deriving (Eq, Ord, Show)

instance HasPositive MaxPositive where
  positive =
    _Wrapped . positive

instance AsPositive MaxPositive where
  _Positive =
    _Wrapped . _Positive

instance MaxPositive ~ a =>
  Rewrapped MaxPositive a
  
instance Wrapped MaxPositive where
  type Unwrapped MaxPositive = Positive
  _Wrapped' =
    iso
      (\(MaxPositive x) -> x)
      MaxPositive

instance Semigroup MaxPositive where
  MaxPositive (Positive x) <> MaxPositive (Positive y) =
    MaxPositive (Positive (x `max` y))

newtype MinPositive =
  MinPositive
    Positive
  deriving (Eq, Ord, Show)

instance HasPositive MinPositive where
  positive =
    _Wrapped . positive

instance AsPositive MinPositive where
  _Positive =
    _Wrapped . _Positive

instance MinPositive ~ a =>
  Rewrapped MinPositive a
  
instance Wrapped MinPositive where
  type Unwrapped MinPositive = Positive
  _Wrapped' =
    iso
      (\(MinPositive x) -> x)
      MinPositive

instance Semigroup MinPositive where
  MinPositive (Positive x) <> MinPositive (Positive y) =
    MinPositive (Positive (x `min` y))

one ::
  Prism'
    Positive
    ()
one =
  prism'
    (\() -> Positive 1)
    (\(Positive n) -> if n == 1 then Just () else Nothing)

one' ::
  Positive
one' =
  one # ()

successor1 ::
  Prism'
    Positive
    Positive
successor1 =
  prism'
    (\(Positive n) -> Positive (n + 1))
    (\(Positive n) -> if n == 1 then Nothing else Just (Positive (n - 1)))

successor1' ::
  Positive
  -> Positive
successor1' =
  (successor1 #)

successorW ::
  Iso'
    Natural
    Positive
successorW =
  iso
    (\(Natural n) -> Positive (n + 1))
    (\(Positive n) -> Natural (n - 1))

plus1 ::
  Positive
  -> Positive
  -> Positive
plus1 x y =
  (_Wrapped # x <> (_Wrapped # y :: SumPositive)) ^. _Wrapped

multiply1 ::
  Positive
  -> Positive
  -> Positive
multiply1 =
  (<>)

square1 ::
  Positive
  -> Positive
  -> Positive
square1 (Positive x) (Positive y) =
  Positive (x ^ y)

oneOr ::
  AsPositive a =>
  a
  -> Positive
oneOr n =
  fromMaybe one' (n ^? _Positive)

notZero ::
  Prism'
    Natural
    Positive
notZero =
  prism'
    (\(Positive n) -> Natural n)
    (\(Natural n) -> if n == 0 then Nothing else Just (Positive n))

length1 ::
  Foldable1 f =>
  f a
  -> Positive
length1 x =
  foldMap1 (const (SumPositive one')) x ^. _Wrapped

replicate1 ::
  Positive
  -> a
  -> NonEmpty a
replicate1 n a =
  take1 n (a :| repeat a)

take1 ::
  Positive
  -> NonEmpty a
  -> NonEmpty a
take1 n (h:|t) =
  h :| take (successorW # n) t

drop1 ::
  Positive
  -> NonEmpty a
  -> [a]
drop1 n (_:|t) =
  drop (successorW # n) t

splitAt1 ::
  Positive
  -> NonEmpty a
  -> (NonEmpty a, [a])
splitAt1 n x =
  (take1 n x, drop1 n x)

(!!!) ::
  NonEmpty a
  -> Positive
  -> Maybe a
(_:|t) !!! n =
  t !! (successorW # n)

findIndices1 ::
  (a -> Bool)
  -> NonEmpty a
  -> [Positive]
findIndices1 p x =
  map snd (NonEmpty.filter (p . fst) (NonEmpty.zip x (NonEmpty.iterate successor1' one')))
  
findIndex1 ::
  (a -> Bool)
  -> NonEmpty a
  -> Maybe Positive
findIndex1 p =
  listToMaybe . findIndices1 p

elemIndices1 ::
  Eq a =>
  a
  -> NonEmpty a
  -> [Positive]
elemIndices1 =
  findIndices1 . (==)

elemIndex1 ::
  Eq a =>
  a
  -> NonEmpty a
  -> Maybe Positive
elemIndex1 =
  findIndex1 . (==)

minus1 ::
  Positive
  -> Positive
  -> Positive
minus1 (Positive x) (Positive y) =
  Positive (if x < y then 1 else x - y)

list1 ::
  Iso'
    Positive
    (NonEmpty ())
list1 =
  iso
    (\n -> replicate1 n ())
    length1

plusone ::
  Natural
  -> Positive
plusone =
  (^. successorW)

minusone ::
  Positive
  -> Natural
minusone =
  (successorW #)
