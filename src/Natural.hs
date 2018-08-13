{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Natural (
  Whole
, HasWhole(..)
, AsWhole(..)
, ProductWhole(..)
, MaxWhole(..)
, MinWhole(..)
, zero
, zero'
, successor
, successor'
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
, Natural
, HasNatural(..)
, AsNatural(..)
, SumNatural(..)
, MaxNatural(..)
, MinNatural(..)
, one
, one'
, successor1
, successor1'
, successorW
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
import Data.Maybe(listToMaybe, Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord((<)), min, max)
import Data.Semigroup(Semigroup((<>)))
import Data.Semigroup.Foldable(Foldable1(foldMap1))
import Data.Tuple(fst, snd)
import Data.Word(Word)
import Prelude(Show, Integral, Integer, (-), (+), (*), fromIntegral)

newtype Whole =
  Whole
    Integer
  deriving (Eq, Ord, Show)

instance Semigroup Whole where
  Whole x <> Whole y =
    Whole (x + y)

instance Monoid Whole where
  mappend =
    (<>)
  mempty =
    Whole 0

class HasWhole a where
  whole ::
    Lens'
      a
      Whole

instance HasWhole Whole where
  whole =
    id

class AsWhole a where
  _Whole ::
    Prism'
      a
      Whole

instance AsWhole Whole where
  _Whole =
    id

integralPrism ::
  Integral a =>
  Prism'
    a
    Whole
integralPrism =
  prism'
    (\(Whole n) -> fromIntegral n)
    (\n -> if n < 0 then Nothing else Just (Whole (fromIntegral n)))

instance AsWhole Int where
  _Whole =
    integralPrism

instance AsWhole Integer where
  _Whole =
    integralPrism

instance AsWhole Word where
  _Whole =
    integralPrism

instance Integral a => AsWhole (Const a b) where
  _Whole =
    integralPrism

instance Integral a => AsWhole (Identity a) where
  _Whole =
    integralPrism

newtype ProductWhole =
  ProductWhole
    Whole
  deriving (Eq, Ord, Show)

instance HasWhole ProductWhole where
  whole =
    _Wrapped . whole

instance AsWhole ProductWhole where
  _Whole =
    _Wrapped . _Whole

instance ProductWhole ~ a =>
  Rewrapped ProductWhole a
  
instance Wrapped ProductWhole where
  type Unwrapped ProductWhole = Whole
  _Wrapped' =
    iso
      (\(ProductWhole x) -> x)
      ProductWhole

instance Semigroup ProductWhole where
  ProductWhole (Whole x) <> ProductWhole (Whole y) =
    ProductWhole (Whole (x * y))

instance Monoid ProductWhole where
  mappend =
    (<>)
  mempty =
    ProductWhole (Whole 1)

newtype MaxWhole =
  MaxWhole
    Whole
  deriving (Eq, Ord, Show)

instance HasWhole MaxWhole where
  whole =
    _Wrapped . whole

instance AsWhole MaxWhole where
  _Whole =
    _Wrapped . _Whole

instance MaxWhole ~ a =>
  Rewrapped MaxWhole a
  
instance Wrapped MaxWhole where
  type Unwrapped MaxWhole = Whole
  _Wrapped' =
    iso
      (\(MaxWhole x) -> x)
      MaxWhole

instance Semigroup MaxWhole where
  MaxWhole (Whole x) <> MaxWhole (Whole y) =
    MaxWhole (Whole (x `max` y))

newtype MinWhole =
  MinWhole
    Whole
  deriving (Eq, Ord, Show)

instance HasWhole MinWhole where
  whole =
    _Wrapped . whole

instance AsWhole MinWhole where
  _Whole =
    _Wrapped . _Whole

instance MinWhole ~ a =>
  Rewrapped MinWhole a
  
instance Wrapped MinWhole where
  type Unwrapped MinWhole = Whole
  _Wrapped' =
    iso
      (\(MinWhole x) -> x)
      MinWhole

instance Semigroup MinWhole where
  MinWhole (Whole x) <> MinWhole (Whole y) =
    MinWhole (Whole (x `min` y))

zero ::
  Prism'
    Whole
    ()
zero =
  prism'
    (\() -> Whole 0)
    (\(Whole n) -> if n == 0 then Nothing else Just ())

zero' ::
  Whole
zero' =
  zero # ()

successor ::
  Prism'
    Whole
    Whole
successor =
  prism'
    (\(Whole n) -> Whole (n + 1))
    (\(Whole n) -> if n == 0 then Nothing else Just (Whole (n - 1)))

successor' ::
  Whole
  -> Whole
successor' =
  (successor #)

length ::
  Foldable f =>
  f a
  -> Whole
length =
  foldl (const . successor') zero'

replicate ::
  Whole
  -> a
  -> [a]
replicate n =
  take n . repeat

take ::
  Whole
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
  Whole
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
  Whole
  -> [a]
  -> ([a], [a])
splitAt n x =
  (take n x, drop n x)

(!!) ::
  [a]
  -> Whole
  -> Maybe a
[] !! _ =
  Nothing
(_:t) !! n =
  (n ^? successor) >>= (t !!)

findIndices ::
  (a -> Bool)
  -> [a]
  -> [Whole]
findIndices p x =
  map snd (filter (p . fst) (zip x (iterate successor' zero')))

findIndex ::
  (a -> Bool)
  -> [a]
  -> Maybe Whole
findIndex p =
  listToMaybe . findIndices p

elemIndices ::
  Eq a =>
  a
  -> [a]
  -> [Whole]
elemIndices =
  findIndices . (==)

elemIndex ::
  Eq a =>
  a
  -> [a]
  -> Maybe Whole
elemIndex =
  findIndex . (==)

minus ::
  Whole
  -> Whole
  -> Whole
minus (Whole x) (Whole y) =
  Whole (if x < y then 0 else x - y)

list ::
  Iso'
    Whole
    [()]
list =
  iso
    (\n -> replicate n ())
    length

----

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

integralPrism1 ::
  Integral a =>
  Prism'
    a
    Natural
integralPrism1 =
  prism'
    (\(Natural n) -> fromIntegral n)
    (\n -> if n < 1 then Nothing else Just (Natural (fromIntegral n)))

instance AsNatural Int where
  _Natural =
    integralPrism1

instance AsNatural Integer where
  _Natural =
    integralPrism1

instance AsNatural Word where
  _Natural =
    integralPrism1

instance Integral a => AsNatural (Const a b) where
  _Natural =
    integralPrism1

instance Integral a => AsNatural (Identity a) where
  _Natural =
    integralPrism1

newtype SumNatural =
  SumNatural
    Natural
  deriving (Eq, Ord, Show)

instance HasNatural SumNatural where
  natural =
    _Wrapped . natural

instance AsNatural SumNatural where
  _Natural =
    _Wrapped . _Natural

instance SumNatural ~ a =>
  Rewrapped SumNatural a
  
instance Wrapped SumNatural where
  type Unwrapped SumNatural = Natural
  _Wrapped' =
    iso
      (\(SumNatural x) -> x)
      SumNatural

instance Semigroup SumNatural where
  SumNatural (Natural x) <> SumNatural (Natural y) =
    SumNatural (Natural (x + y))

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

one ::
  Prism'
    Natural
    ()
one =
  prism'
    (\() -> Natural 1)
    (\(Natural n) -> if n == 1 then Nothing else Just ())

one' ::
  Natural
one' =
  one # ()

successor1 ::
  Prism'
    Natural
    Natural
successor1 =
  prism'
    (\(Natural n) -> Natural (n + 1))
    (\(Natural n) -> if n == 1 then Nothing else Just (Natural (n - 1)))

successor1' ::
  Natural
  -> Natural
successor1' =
  (successor1 #)

successorW ::
  Iso'
    Whole
    Natural
successorW =
  iso
    (\(Whole n) -> Natural (n + 1))
    (\(Natural n) -> Whole (n - 1))

notZero ::
  Prism'
    Whole
    Natural
notZero =
  prism'
    (\(Natural n) -> Whole n)
    (\(Whole n) -> if n == 0 then Nothing else Just (Natural n))

length1 ::
  Foldable1 f =>
  f a
  -> Natural
length1 x =
  foldMap1 (const (SumNatural one')) x ^. _Wrapped

replicate1 ::
  Natural
  -> a
  -> NonEmpty a
replicate1 n a =
  take1 n (a :| repeat a)

take1 ::
  Natural
  -> NonEmpty a
  -> NonEmpty a
take1 n (h:|t) =
  h :| take (successorW # n) t

drop1 ::
  Natural
  -> NonEmpty a
  -> [a]
drop1 n (_:|t) =
  drop (successorW # n) t

splitAt1 ::
  Natural
  -> NonEmpty a
  -> (NonEmpty a, [a])
splitAt1 n x =
  (take1 n x, drop1 n x)

(!!!) ::
  NonEmpty a
  -> Natural
  -> Maybe a
(_:|t) !!! n =
  t !! (successorW # n)

findIndices1 ::
  (a -> Bool)
  -> NonEmpty a
  -> [Natural]
findIndices1 p x =
  map snd (NonEmpty.filter (p . fst) (NonEmpty.zip x (NonEmpty.iterate successor1' one')))
  
findIndex1 ::
  (a -> Bool)
  -> NonEmpty a
  -> Maybe Natural
findIndex1 p =
  listToMaybe . findIndices1 p

elemIndices1 ::
  Eq a =>
  a
  -> NonEmpty a
  -> [Natural]
elemIndices1 =
  findIndices1 . (==)

elemIndex1 ::
  Eq a =>
  a
  -> NonEmpty a
  -> Maybe Natural
elemIndex1 =
  findIndex1 . (==)

minus1 ::
  Natural
  -> Natural
  -> Natural
minus1 (Natural x) (Natural y) =
  Natural (if x < y then 1 else x - y)

list1 ::
  Iso'
    Natural
    (NonEmpty ())
list1 =
  iso
    (\n -> replicate1 n ())
    length1
