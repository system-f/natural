{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Whole(
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
) where

import Control.Applicative(Const)
import Control.Category((.), id)
import Control.Lens(Wrapped(_Wrapped', Unwrapped), Rewrapped, Prism', Lens', Iso', (^?), ( # ), _Wrapped, prism', iso)
import Control.Monad((>>=))
import Data.Bool(Bool)
import Data.Eq(Eq((==)))
import Data.Foldable(Foldable(foldl))
import Data.Function(const)
import Data.Functor.Identity(Identity)
import Data.Int(Int)
import Data.List(iterate, zip, filter, map, repeat)
import Data.Maybe(listToMaybe, Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord((<)), min, max)
import Data.Semigroup(Semigroup((<>)))
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
