{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Natural(
  Natural
, HasNatural(..)
, AsNatural(..)
, MaxNatural(..)
, MinNatural(..)
, one
, one'
, successor
, successor'
, replicate
, take
, drop
, splitAt
, (!!)
, minus
, list1
) where

import Control.Applicative(Const)
import Control.Category((.), id)
import Control.Lens(Wrapped(_Wrapped', Unwrapped), Rewrapped, Prism', Lens', Iso', (^?), (^.), ( # ), _Wrapped, prism', iso)
import Control.Monad((>>=))
import Data.Bool(Bool)
import Data.Eq(Eq((==)))
import Data.Foldable(Foldable(foldl))
import Data.Function(const)
import Data.Functor.Identity(Identity)
import Data.Int(Int)
import Data.List(iterate, zip, filter, map, repeat)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(listToMaybe, Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord((<)), min, max)
import Data.Semigroup(Semigroup((<>)))
import Data.Semigroup.Foldable
import Data.Tuple(fst, snd)
import Data.Word(Word)
import Prelude(Show, Integral, Integer, (-), (+), (*), fromIntegral)
import Whole

newtype Natural =
  Natural
    Integer
  deriving (Eq, Ord, Show)

instance Semigroup Natural where
  Natural x <> Natural y =
    Natural (x * y)

instance Monoid Natural where
  mappend =
    (<>)
  mempty =
    Natural 1

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
    (\n -> if n < 1 then Nothing else Just (Natural (fromIntegral n)))

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

successor ::
  Prism'
    Natural
    Natural
successor =
  prism'
    (\(Natural n) -> Natural (n + 1))
    (\(Natural n) -> if n == 1 then Nothing else Just (Natural (n - 1)))

successor' ::
  Natural
  -> Natural
successor' =
  (successor #)

length ::
  Foldable1 f =>
  f a
  -> Natural
length x =
  foldMap1 (\_ -> SumNatural one') x ^. _Wrapped

replicate ::
  Natural
  -> a
  -> [a]
replicate n =
  take n . repeat

take' ::
  Natural
  -> NonEmpty a
  -> NonEmpty a
take' n (h:|t) =
  undefined
  {-
  case n ^? successor of
    Nothing ->
      []
    Just p ->
      h :| take' p t
  -}


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

minus ::
  Natural
  -> Natural
  -> Natural
minus (Natural x) (Natural y) =
  Natural (if x < y then 1 else x - y)

list1 ::
  Iso'
    Natural
    (NonEmpty ())
list1 =
  iso
    undefined -- (\n -> replicate n ())
    undefined -- (foldl (const . successor') one')

undefined = undefined