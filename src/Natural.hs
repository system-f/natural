{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Natural
  ( Natural,
    HasNatural (..),
    AsNatural (..),
    ProductNatural (..),
    MaxNatural (..),
    MinNatural (..),
    toJsonNatural,
    parseJsonNatural,
    zero,
    zero',
    successor,
    successor',
    plus,
    multiply,
    power,
    zeroOr,
    length,
    replicate,
    take,
    drop,
    splitAt,
    (!!),
    findIndices,
    findIndex,
    elemIndices,
    elemIndex,
    minus,
    list,
    Positive,
    HasPositive (..),
    AsPositive (..),
    SumPositive (..),
    MaxPositive (..),
    MinPositive (..),
    toJsonPositive,
    parseJsonPositive,
    naturalPositive,
    one,
    one',
    successor1,
    successor1',
    successorW,
    plus1,
    multiply1,
    power1,
    oneOr,
    notZero,
    length1,
    replicate1,
    take1,
    drop1,
    splitAt1,
    (!!!),
    findIndices1,
    findIndex1,
    elemIndices1,
    elemIndex1,
    minus1,
    list1,
    plusone,
    minusone,
  )
where

import Control.Applicative (Const, pure)
import Control.Category (id, (.))
import Control.Lens (Iso', Lens', Prism', Rewrapped, Wrapped (Unwrapped, _Wrapped'), iso, prism', view, (#), (^.), (^?), _Wrapped)
import Control.Monad ((>>=))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    Parser,
    ToJSON (toEncoding, toJSON),
    Value,
  )
import Data.Bool (Bool)
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable (foldl))
import Data.Function (const)
import Data.Functor.Identity (Identity)
import Data.Int (Int)
import Data.List (filter, iterate, map, repeat, zip)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty (filter, iterate, zip)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (Monoid (mappend, mempty))
import Data.Ord (Ord ((<), (<=)), max, min)
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Tuple (fst, snd)
import Data.Word (Word)
import Prelude (Integer, Integral, Show, fail, fromIntegral, (*), (+), (-), (^))

-- $setup
-- >>> import Data.Aeson
-- >>> import Data.Aeson.Types (parse)
-- >>> :set -XOverloadedStrings

-- | A natural number (non-negative integer: 0, 1, 2, ...).
--
-- >>> Natural 0
-- Natural 0
--
-- >>> Natural 42
-- Natural 42
newtype Natural
  = Natural
      Integer
  deriving (Eq, Ord, Show)

-- | Natural numbers form a semigroup under addition.
--
-- >>> Natural 3 <> Natural 4
-- Natural 7
instance Semigroup Natural where
  Natural x <> Natural y =
    Natural (x + y)

-- | The additive monoid of natural numbers with identity 0.
--
-- >>> mempty :: Natural
-- Natural 0
--
-- >>> mappend (Natural 3) (Natural 4)
-- Natural 7
instance Monoid Natural where
  mappend =
    (<>)
  mempty =
    Natural 0

-- | Typeclass for types that have a 'Natural' lens.
--
-- >>> Natural 5 ^. natural
-- Natural 5
class HasNatural a where
  natural ::
    Lens'
      a
      Natural

-- | >>> Natural 5 ^. natural
-- Natural 5
instance HasNatural Natural where
  natural =
    id

-- | Typeclass for types that can be converted to/from 'Natural' via a prism.
--
-- >>> (5 :: Integer) ^? _Natural
-- Just (Natural 5)
--
-- >>> (-1 :: Integer) ^? _Natural
-- Nothing
class AsNatural a where
  _Natural ::
    Prism'
      a
      Natural

-- | >>> Natural 5 ^? _Natural
-- Just (Natural 5)
instance AsNatural Natural where
  _Natural =
    id

-- | Prism for converting an integral value to a 'Natural'.
--
-- >>> (5 :: Integer) ^? integralPrism
-- Just (Natural 5)
--
-- >>> (-1 :: Integer) ^? integralPrism
-- Nothing
integralPrism ::
  (Integral a) =>
  Prism'
    a
    Natural
integralPrism =
  prism'
    (\(Natural n) -> fromIntegral n)
    (\n -> if n < 0 then Nothing else Just (Natural (fromIntegral n)))

-- | >>> (5 :: Int) ^? _Natural
-- Just (Natural 5)
--
-- >>> (-1 :: Int) ^? _Natural
-- Nothing
instance AsNatural Int where
  _Natural =
    integralPrism

-- | >>> (5 :: Integer) ^? _Natural
-- Just (Natural 5)
--
-- >>> (-1 :: Integer) ^? _Natural
-- Nothing
instance AsNatural Integer where
  _Natural =
    integralPrism

-- | >>> (5 :: Word) ^? _Natural
-- Just (Natural 5)
instance AsNatural Word where
  _Natural =
    integralPrism

-- | >>> (Const 5 :: Const Integer Bool) ^? _Natural
-- Just (Natural 5)
instance (Integral a) => AsNatural (Const a b) where
  _Natural =
    integralPrism

-- | >>> (Identity 5 :: Identity Integer) ^? _Natural
-- Just (Natural 5)
instance (Integral a) => AsNatural (Identity a) where
  _Natural =
    integralPrism

-- | Natural numbers under multiplication.
--
-- >>> ProductNatural (Natural 3) <> ProductNatural (Natural 4)
-- ProductNatural (Natural 12)
--
-- >>> mempty :: ProductNatural
-- ProductNatural (Natural 1)
newtype ProductNatural
  = ProductNatural
      Natural
  deriving (Eq, Ord, Show)

-- | >>> ProductNatural (Natural 5) ^. natural
-- Natural 5
instance HasNatural ProductNatural where
  natural =
    _Wrapped . natural

-- | >>> ProductNatural (Natural 5) ^? _Natural
-- Just (Natural 5)
instance AsNatural ProductNatural where
  _Natural =
    _Wrapped . _Natural

instance
  (ProductNatural ~ a) =>
  Rewrapped ProductNatural a

-- | >>> ProductNatural (Natural 5) ^. _Wrapped'
-- Natural 5
instance Wrapped ProductNatural where
  type Unwrapped ProductNatural = Natural
  _Wrapped' =
    iso
      (\(ProductNatural x) -> x)
      ProductNatural

-- | >>> ProductNatural (Natural 3) <> ProductNatural (Natural 4)
-- ProductNatural (Natural 12)
instance Semigroup ProductNatural where
  ProductNatural (Natural x) <> ProductNatural (Natural y) =
    ProductNatural (Natural (x * y))

-- | >>> mempty :: ProductNatural
-- ProductNatural (Natural 1)
instance Monoid ProductNatural where
  mappend =
    (<>)
  mempty =
    ProductNatural (Natural 1)

-- | Natural numbers under maximum.
--
-- >>> MaxNatural (Natural 3) <> MaxNatural (Natural 7)
-- MaxNatural (Natural 7)
newtype MaxNatural
  = MaxNatural
      Natural
  deriving (Eq, Ord, Show)

-- | >>> MaxNatural (Natural 5) ^. natural
-- Natural 5
instance HasNatural MaxNatural where
  natural =
    _Wrapped . natural

-- | >>> MaxNatural (Natural 5) ^? _Natural
-- Just (Natural 5)
instance AsNatural MaxNatural where
  _Natural =
    _Wrapped . _Natural

instance
  (MaxNatural ~ a) =>
  Rewrapped MaxNatural a

-- | >>> MaxNatural (Natural 5) ^. _Wrapped'
-- Natural 5
instance Wrapped MaxNatural where
  type Unwrapped MaxNatural = Natural
  _Wrapped' =
    iso
      (\(MaxNatural x) -> x)
      MaxNatural

-- | >>> MaxNatural (Natural 3) <> MaxNatural (Natural 7)
-- MaxNatural (Natural 7)
instance Semigroup MaxNatural where
  MaxNatural (Natural x) <> MaxNatural (Natural y) =
    MaxNatural (Natural (x `max` y))

-- | Natural numbers under minimum.
--
-- >>> MinNatural (Natural 3) <> MinNatural (Natural 7)
-- MinNatural (Natural 3)
newtype MinNatural
  = MinNatural
      Natural
  deriving (Eq, Ord, Show)

-- | >>> MinNatural (Natural 5) ^. natural
-- Natural 5
instance HasNatural MinNatural where
  natural =
    _Wrapped . natural

-- | >>> MinNatural (Natural 5) ^? _Natural
-- Just (Natural 5)
instance AsNatural MinNatural where
  _Natural =
    _Wrapped . _Natural

instance
  (MinNatural ~ a) =>
  Rewrapped MinNatural a

-- | >>> MinNatural (Natural 5) ^. _Wrapped'
-- Natural 5
instance Wrapped MinNatural where
  type Unwrapped MinNatural = Natural
  _Wrapped' =
    iso
      (\(MinNatural x) -> x)
      MinNatural

-- | >>> MinNatural (Natural 3) <> MinNatural (Natural 7)
-- MinNatural (Natural 3)
instance Semigroup MinNatural where
  MinNatural (Natural x) <> MinNatural (Natural y) =
    MinNatural (Natural (x `min` y))

-- | Serialises a 'Natural' to a JSON number.
--
-- >>> toJSON (Natural 0)
-- Number 0.0
--
-- >>> toJSON (Natural 42)
-- Number 42.0
--
-- >>> encode (Natural 0)
-- "0"
--
-- >>> encode (Natural 42)
-- "42"
instance ToJSON Natural where
  toJSON =
    toJsonNatural
  toEncoding (Natural n) =
    toEncoding n

-- | Serialises any value with a 'HasNatural' instance to a JSON 'Value'.
--
-- >>> toJsonNatural (Natural 42)
-- Number 42.0
--
-- >>> toJsonNatural (ProductNatural (Natural 12))
-- Number 12.0
--
-- >>> toJsonNatural (MaxNatural (Natural 7))
-- Number 7.0
--
-- >>> toJsonNatural (MinNatural (Natural 3))
-- Number 3.0
{-# SPECIALIZE toJsonNatural ::
  Natural ->
  Value
  #-}
{-# INLINE toJsonNatural #-}
toJsonNatural ::
  (HasNatural a) =>
  a ->
  Value
toJsonNatural a =
  let Natural n = view natural a
   in toJSON n

-- | Parses a 'Natural' from a JSON number, failing on negative values.
--
-- >>> fromJSON (Number 0) :: Result Natural
-- Success (Natural 0)
--
-- >>> fromJSON (Number 42) :: Result Natural
-- Success (Natural 42)
--
-- >>> decode "42" :: Maybe Natural
-- Just (Natural 42)
--
-- >>> decode "0" :: Maybe Natural
-- Just (Natural 0)
--
-- >>> decode "-1" :: Maybe Natural
-- Nothing
instance FromJSON Natural where
  parseJSON =
    parseJsonNatural

-- | Parses a JSON value into a 'Natural', failing on negative values.
--
-- >>> parse parseJsonNatural (Number 42)
-- Success (Natural 42)
--
-- >>> parse parseJsonNatural (Number 0)
-- Success (Natural 0)
--
-- >>> parse parseJsonNatural (Number (-1))
-- Error "parse failed, Natural: expected non-negative integer"
{-# INLINE parseJsonNatural #-}
parseJsonNatural ::
  Value ->
  Parser Natural
parseJsonNatural v =
  parseJSON v >>= \n ->
    if n < 0
      then fail "parse failed, Natural: expected non-negative integer"
      else pure (Natural n)

-- | Serialises a 'ProductNatural' to a JSON number.
--
-- >>> encode (ProductNatural (Natural 12))
-- "12"
instance ToJSON ProductNatural where
  toJSON =
    toJsonNatural
  toEncoding (ProductNatural n) =
    toEncoding n

-- | Parses a 'ProductNatural' from a JSON number, failing on negative values.
--
-- >>> decode "12" :: Maybe ProductNatural
-- Just (ProductNatural (Natural 12))
--
-- >>> decode "-1" :: Maybe ProductNatural
-- Nothing
instance FromJSON ProductNatural where
  parseJSON v =
    parseJsonNatural v >>= \n -> pure (ProductNatural n)

-- | Serialises a 'MaxNatural' to a JSON number.
--
-- >>> encode (MaxNatural (Natural 7))
-- "7"
instance ToJSON MaxNatural where
  toJSON =
    toJsonNatural
  toEncoding (MaxNatural n) =
    toEncoding n

-- | Parses a 'MaxNatural' from a JSON number, failing on negative values.
--
-- >>> decode "7" :: Maybe MaxNatural
-- Just (MaxNatural (Natural 7))
--
-- >>> decode "-1" :: Maybe MaxNatural
-- Nothing
instance FromJSON MaxNatural where
  parseJSON v =
    parseJsonNatural v >>= \n -> pure (MaxNatural n)

-- | Serialises a 'MinNatural' to a JSON number.
--
-- >>> encode (MinNatural (Natural 3))
-- "3"
instance ToJSON MinNatural where
  toJSON =
    toJsonNatural
  toEncoding (MinNatural n) =
    toEncoding n

-- | Parses a 'MinNatural' from a JSON number, failing on negative values.
--
-- >>> decode "3" :: Maybe MinNatural
-- Just (MinNatural (Natural 3))
--
-- >>> decode "-1" :: Maybe MinNatural
-- Nothing
instance FromJSON MinNatural where
  parseJSON v =
    parseJsonNatural v >>= \n -> pure (MinNatural n)

-- | A prism for the zero natural number.
--
-- >>> zero # ()
-- Natural 0
--
-- >>> Natural 0 ^? zero
-- Just ()
--
-- >>> Natural 5 ^? zero
-- Nothing
zero ::
  Prism'
    Natural
    ()
zero =
  prism'
    (\() -> Natural 0)
    (\(Natural n) -> if n == 0 then Just () else Nothing)

-- | The zero natural number.
--
-- >>> zero'
-- Natural 0
zero' ::
  Natural
zero' =
  zero # ()

-- | A prism for the successor of a natural number.
--
-- >>> successor # Natural 0
-- Natural 1
--
-- >>> successor # Natural 4
-- Natural 5
--
-- >>> Natural 5 ^? successor
-- Just (Natural 4)
--
-- >>> Natural 0 ^? successor
-- Nothing
successor ::
  Prism'
    Natural
    Natural
successor =
  prism'
    (\(Natural n) -> Natural (n + 1))
    (\(Natural n) -> if n == 0 then Nothing else Just (Natural (n - 1)))

-- | The successor of a natural number.
--
-- >>> successor' (Natural 0)
-- Natural 1
--
-- >>> successor' (Natural 4)
-- Natural 5
successor' ::
  Natural ->
  Natural
successor' =
  (successor #)

-- | Add two natural numbers.
--
-- >>> plus (Natural 3) (Natural 4)
-- Natural 7
--
-- >>> plus (Natural 0) (Natural 5)
-- Natural 5
plus ::
  Natural ->
  Natural ->
  Natural
plus =
  (<>)

-- | Multiply two natural numbers.
--
-- >>> multiply (Natural 3) (Natural 4)
-- Natural 12
--
-- >>> multiply (Natural 0) (Natural 5)
-- Natural 0
multiply ::
  Natural ->
  Natural ->
  Natural
multiply x y =
  (_Wrapped # x <> (_Wrapped # y :: ProductNatural)) ^. _Wrapped

-- | Raise a natural number to a power.
--
-- >>> power (Natural 2) (Natural 10)
-- Natural 1024
--
-- >>> power (Natural 5) (Natural 0)
-- Natural 1
power ::
  Natural ->
  Natural ->
  Natural
power (Natural x) (Natural y) =
  Natural (x ^ y)

-- | Convert to 'Natural', defaulting to zero if the value is not a valid natural.
--
-- >>> zeroOr (5 :: Integer)
-- Natural 5
--
-- >>> zeroOr (-1 :: Integer)
-- Natural 0
zeroOr ::
  (AsNatural a) =>
  a ->
  Natural
zeroOr n =
  fromMaybe zero' (n ^? _Natural)

-- | The length of a foldable structure as a 'Natural'.
--
-- >>> length [1, 2, 3 :: Int]
-- Natural 3
--
-- >>> length ([] :: [Int])
-- Natural 0
length ::
  (Foldable f) =>
  f a ->
  Natural
length =
  foldl (const . successor') zero'

-- | Replicate a value a natural number of times.
--
-- >>> replicate (Natural 3) 'x'
-- "xxx"
--
-- >>> replicate (Natural 0) 'x'
-- ""
replicate ::
  Natural ->
  a ->
  [a]
replicate n =
  take n . repeat

-- | Take a natural number of elements from a list.
--
-- >>> take (Natural 2) [1, 2, 3 :: Int]
-- [1,2]
--
-- >>> take (Natural 0) [1, 2, 3 :: Int]
-- []
--
-- >>> take (Natural 5) [1, 2, 3 :: Int]
-- [1,2,3]
take ::
  Natural ->
  [a] ->
  [a]
take _ [] =
  []
take n (h : t) =
  case n ^? successor of
    Nothing ->
      []
    Just p ->
      h : take p t

-- | Drop a natural number of elements from a list.
--
-- >>> drop (Natural 2) [1, 2, 3, 4 :: Int]
-- [3,4]
--
-- >>> drop (Natural 0) [1, 2, 3 :: Int]
-- [1,2,3]
--
-- >>> drop (Natural 5) [1, 2, 3 :: Int]
-- []
drop ::
  Natural ->
  [a] ->
  [a]
drop _ [] =
  []
drop n (h : t) =
  case n ^? successor of
    Nothing ->
      h : t
    Just p ->
      drop p t

-- | Split a list at a natural index.
--
-- >>> splitAt (Natural 2) [1, 2, 3, 4 :: Int]
-- ([1,2],[3,4])
--
-- >>> splitAt (Natural 0) [1, 2, 3 :: Int]
-- ([],[1,2,3])
--
-- >>> splitAt (Natural 5) [1, 2, 3 :: Int]
-- ([1,2,3],[])
splitAt ::
  Natural ->
  [a] ->
  ([a], [a])
splitAt n x =
  (take n x, drop n x)

-- | Safe list indexing by 'Natural'.
--
-- >>> ([] :: [Int]) !! Natural 0
-- Nothing
--
-- >>> [1, 2, 3 :: Int] !! Natural 0
-- Just 1
(!!) ::
  [a] ->
  Natural ->
  Maybe a
[] !! _ =
  Nothing
(h : t) !! n = case n ^? successor of
  Nothing -> Just h
  Just p  -> t !! p

-- | Find all indices where a predicate holds.
--
-- >>> findIndices (== 'a') "abacus"
-- [Natural 0,Natural 2]
--
-- >>> findIndices (== 'z') "abc"
-- []
findIndices ::
  (a -> Bool) ->
  [a] ->
  [Natural]
findIndices p x =
  map snd (filter (p . fst) (zip x (iterate successor' zero')))

-- | Find the first index where a predicate holds.
--
-- >>> findIndex (== 'a') "banana"
-- Just (Natural 1)
--
-- >>> findIndex (== 'z') "banana"
-- Nothing
findIndex ::
  (a -> Bool) ->
  [a] ->
  Maybe Natural
findIndex p =
  listToMaybe . findIndices p

-- | Find all indices of an element.
--
-- >>> elemIndices 'a' "abacus"
-- [Natural 0,Natural 2]
--
-- >>> elemIndices 'z' "abc"
-- []
elemIndices ::
  (Eq a) =>
  a ->
  [a] ->
  [Natural]
elemIndices =
  findIndices . (==)

-- | Find the first index of an element.
--
-- >>> elemIndex 'a' "banana"
-- Just (Natural 1)
--
-- >>> elemIndex 'z' "banana"
-- Nothing
elemIndex ::
  (Eq a) =>
  a ->
  [a] ->
  Maybe Natural
elemIndex =
  findIndex . (==)

-- | Subtract two natural numbers, flooring at zero.
--
-- >>> minus (Natural 5) (Natural 3)
-- Natural 2
--
-- >>> minus (Natural 3) (Natural 5)
-- Natural 0
minus ::
  Natural ->
  Natural ->
  Natural
minus (Natural x) (Natural y) =
  Natural (if x < y then 0 else x - y)

-- | An isomorphism between 'Natural' and @[()]@.
--
-- >>> Natural 3 ^. list
-- [(),(),()]
--
-- >>> Natural 0 ^. list
-- []
--
-- >>> list # [(), (), ()]
-- Natural 3
list ::
  Iso'
    Natural
    [()]
list =
  iso
    (`replicate` ())
    length

----

-- | A positive integer (strictly positive: 1, 2, 3, ...).
--
-- >>> Positive 1
-- Positive 1
--
-- >>> Positive 42
-- Positive 42
newtype Positive
  = Positive
      Integer
  deriving (Eq, Ord, Show)

-- | Positive numbers form a semigroup under addition.
--
-- >>> Positive 3 <> Positive 4
-- Positive 7
instance Semigroup Positive where
  Positive x <> Positive y =
    Positive (x + y)

-- | Typeclass for types that have a 'Positive' lens.
--
-- >>> Positive 5 ^. positive
-- Positive 5
class HasPositive a where
  positive ::
    Lens'
      a
      Positive

-- | >>> Positive 5 ^. positive
-- Positive 5
instance HasPositive Positive where
  positive =
    id

-- | Typeclass for types that can be converted to/from 'Positive' via a prism.
--
-- >>> (5 :: Integer) ^? _Positive
-- Just (Positive 5)
--
-- >>> (0 :: Integer) ^? _Positive
-- Nothing
class AsPositive a where
  _Positive ::
    Prism'
      a
      Positive

-- | >>> Positive 5 ^? _Positive
-- Just (Positive 5)
instance AsPositive Positive where
  _Positive =
    id

-- | Prism for converting an integral value to a 'Positive'.
--
-- >>> (5 :: Integer) ^? integralPrism1
-- Just (Positive 5)
--
-- >>> (0 :: Integer) ^? integralPrism1
-- Nothing
integralPrism1 ::
  (Integral a) =>
  Prism'
    a
    Positive
integralPrism1 =
  prism'
    (\(Positive n) -> fromIntegral n)
    (\n -> if n < 1 then Nothing else Just (Positive (fromIntegral n)))

-- | >>> (5 :: Int) ^? _Positive
-- Just (Positive 5)
--
-- >>> (0 :: Int) ^? _Positive
-- Nothing
instance AsPositive Int where
  _Positive =
    integralPrism1

-- | >>> (5 :: Integer) ^? _Positive
-- Just (Positive 5)
--
-- >>> (0 :: Integer) ^? _Positive
-- Nothing
instance AsPositive Integer where
  _Positive =
    integralPrism1

-- | >>> (5 :: Word) ^? _Positive
-- Just (Positive 5)
--
-- >>> (0 :: Word) ^? _Positive
-- Nothing
instance AsPositive Word where
  _Positive =
    integralPrism1

-- | >>> (Const 5 :: Const Integer Bool) ^? _Positive
-- Just (Positive 5)
instance (Integral a) => AsPositive (Const a b) where
  _Positive =
    integralPrism1

-- | >>> (Identity 5 :: Identity Integer) ^? _Positive
-- Just (Positive 5)
instance (Integral a) => AsPositive (Identity a) where
  _Positive =
    integralPrism1

-- | Positive numbers under addition.
--
-- >>> SumPositive (Positive 3) <> SumPositive (Positive 4)
-- SumPositive (Positive 7)
newtype SumPositive
  = SumPositive
      Positive
  deriving (Eq, Ord, Show)

-- | >>> SumPositive (Positive 5) ^. positive
-- Positive 5
instance HasPositive SumPositive where
  positive =
    _Wrapped . positive

-- | >>> SumPositive (Positive 5) ^? _Positive
-- Just (Positive 5)
instance AsPositive SumPositive where
  _Positive =
    _Wrapped . _Positive

instance
  (SumPositive ~ a) =>
  Rewrapped SumPositive a

-- | >>> SumPositive (Positive 5) ^. _Wrapped'
-- Positive 5
instance Wrapped SumPositive where
  type Unwrapped SumPositive = Positive
  _Wrapped' =
    iso
      (\(SumPositive x) -> x)
      SumPositive

-- | >>> SumPositive (Positive 3) <> SumPositive (Positive 4)
-- SumPositive (Positive 7)
instance Semigroup SumPositive where
  SumPositive (Positive x) <> SumPositive (Positive y) =
    SumPositive (Positive (x + y))

-- | Positive numbers under maximum.
--
-- >>> MaxPositive (Positive 3) <> MaxPositive (Positive 7)
-- MaxPositive (Positive 7)
newtype MaxPositive
  = MaxPositive
      Positive
  deriving (Eq, Ord, Show)

-- | >>> MaxPositive (Positive 5) ^. positive
-- Positive 5
instance HasPositive MaxPositive where
  positive =
    _Wrapped . positive

-- | >>> MaxPositive (Positive 5) ^? _Positive
-- Just (Positive 5)
instance AsPositive MaxPositive where
  _Positive =
    _Wrapped . _Positive

instance
  (MaxPositive ~ a) =>
  Rewrapped MaxPositive a

-- | >>> MaxPositive (Positive 5) ^. _Wrapped'
-- Positive 5
instance Wrapped MaxPositive where
  type Unwrapped MaxPositive = Positive
  _Wrapped' =
    iso
      (\(MaxPositive x) -> x)
      MaxPositive

-- | >>> MaxPositive (Positive 3) <> MaxPositive (Positive 7)
-- MaxPositive (Positive 7)
instance Semigroup MaxPositive where
  MaxPositive (Positive x) <> MaxPositive (Positive y) =
    MaxPositive (Positive (x `max` y))

-- | Positive numbers under minimum.
--
-- >>> MinPositive (Positive 3) <> MinPositive (Positive 7)
-- MinPositive (Positive 3)
newtype MinPositive
  = MinPositive
      Positive
  deriving (Eq, Ord, Show)

-- | >>> MinPositive (Positive 5) ^. positive
-- Positive 5
instance HasPositive MinPositive where
  positive =
    _Wrapped . positive

-- | >>> MinPositive (Positive 5) ^? _Positive
-- Just (Positive 5)
instance AsPositive MinPositive where
  _Positive =
    _Wrapped . _Positive

instance
  (MinPositive ~ a) =>
  Rewrapped MinPositive a

-- | >>> MinPositive (Positive 5) ^. _Wrapped'
-- Positive 5
instance Wrapped MinPositive where
  type Unwrapped MinPositive = Positive
  _Wrapped' =
    iso
      (\(MinPositive x) -> x)
      MinPositive

-- | >>> MinPositive (Positive 3) <> MinPositive (Positive 7)
-- MinPositive (Positive 3)
instance Semigroup MinPositive where
  MinPositive (Positive x) <> MinPositive (Positive y) =
    MinPositive (Positive (x `min` y))

-- | Serialises a 'Positive' to a JSON number.
--
-- >>> toJSON (Positive 1)
-- Number 1.0
--
-- >>> toJSON (Positive 42)
-- Number 42.0
--
-- >>> encode (Positive 1)
-- "1"
--
-- >>> encode (Positive 42)
-- "42"
instance ToJSON Positive where
  toJSON =
    toJsonPositive
  toEncoding (Positive n) =
    toEncoding n

-- | Serialises any value with a 'HasPositive' instance to a JSON 'Value'.
--
-- >>> toJsonPositive (Positive 42)
-- Number 42.0
--
-- >>> toJsonPositive (SumPositive (Positive 7))
-- Number 7.0
--
-- >>> toJsonPositive (MaxPositive (Positive 7))
-- Number 7.0
--
-- >>> toJsonPositive (MinPositive (Positive 3))
-- Number 3.0
{-# SPECIALIZE toJsonPositive ::
  Positive ->
  Value
  #-}
{-# INLINE toJsonPositive #-}
toJsonPositive ::
  (HasPositive a) =>
  a ->
  Value
toJsonPositive a =
  let Positive n = view positive a
   in toJSON n

-- | Parses a 'Positive' from a JSON number, failing on non-positive values.
--
-- >>> fromJSON (Number 1) :: Result Positive
-- Success (Positive 1)
--
-- >>> fromJSON (Number 42) :: Result Positive
-- Success (Positive 42)
--
-- >>> decode "42" :: Maybe Positive
-- Just (Positive 42)
--
-- >>> decode "1" :: Maybe Positive
-- Just (Positive 1)
--
-- >>> decode "0" :: Maybe Positive
-- Nothing
--
-- >>> decode "-1" :: Maybe Positive
-- Nothing
instance FromJSON Positive where
  parseJSON =
    parseJsonPositive

-- | Parses a JSON value into a 'Positive', failing on non-positive values.
--
-- >>> parse parseJsonPositive (Number 42)
-- Success (Positive 42)
--
-- >>> parse parseJsonPositive (Number 1)
-- Success (Positive 1)
--
-- >>> parse parseJsonPositive (Number 0)
-- Error "parse failed, Positive: expected positive integer"
--
-- >>> parse parseJsonPositive (Number (-1))
-- Error "parse failed, Positive: expected positive integer"
{-# INLINE parseJsonPositive #-}
parseJsonPositive ::
  Value ->
  Parser Positive
parseJsonPositive v =
  parseJSON v >>= \n ->
    if n < 1
      then fail "parse failed, Positive: expected positive integer"
      else pure (Positive n)

-- | Serialises a 'SumPositive' to a JSON number.
--
-- >>> encode (SumPositive (Positive 7))
-- "7"
instance ToJSON SumPositive where
  toJSON =
    toJsonPositive
  toEncoding (SumPositive n) =
    toEncoding n

-- | Parses a 'SumPositive' from a JSON number, failing on non-positive values.
--
-- >>> decode "7" :: Maybe SumPositive
-- Just (SumPositive (Positive 7))
--
-- >>> decode "0" :: Maybe SumPositive
-- Nothing
instance FromJSON SumPositive where
  parseJSON v =
    parseJsonPositive v >>= \n -> pure (SumPositive n)

-- | Serialises a 'MaxPositive' to a JSON number.
--
-- >>> encode (MaxPositive (Positive 7))
-- "7"
instance ToJSON MaxPositive where
  toJSON =
    toJsonPositive
  toEncoding (MaxPositive n) =
    toEncoding n

-- | Parses a 'MaxPositive' from a JSON number, failing on non-positive values.
--
-- >>> decode "7" :: Maybe MaxPositive
-- Just (MaxPositive (Positive 7))
--
-- >>> decode "0" :: Maybe MaxPositive
-- Nothing
instance FromJSON MaxPositive where
  parseJSON v =
    parseJsonPositive v >>= \n -> pure (MaxPositive n)

-- | Serialises a 'MinPositive' to a JSON number.
--
-- >>> encode (MinPositive (Positive 3))
-- "3"
instance ToJSON MinPositive where
  toJSON =
    toJsonPositive
  toEncoding (MinPositive n) =
    toEncoding n

-- | Parses a 'MinPositive' from a JSON number, failing on non-positive values.
--
-- >>> decode "3" :: Maybe MinPositive
-- Just (MinPositive (Positive 3))
--
-- >>> decode "0" :: Maybe MinPositive
-- Nothing
instance FromJSON MinPositive where
  parseJSON v =
    parseJsonPositive v >>= \n -> pure (MinPositive n)

-- | An isomorphism between 'Natural' and 'Maybe Positive'.
--
-- >>> Natural 0 ^. naturalPositive
-- Nothing
--
-- >>> Natural 5 ^. naturalPositive
-- Just (Positive 5)
--
-- >>> naturalPositive # Nothing
-- Natural 0
--
-- >>> naturalPositive # Just (Positive 3)
-- Natural 3
naturalPositive ::
  Iso' Natural (Maybe Positive)
naturalPositive =
  iso
    ( \(Natural n) ->
        if n == 0 then Nothing else Just (Positive n)
    )
    ( \x ->
        Natural
          ( case x of
              Nothing ->
                0
              Just (Positive n) ->
                n
          )
    )

-- | >>> Natural 5 ^? _Positive
-- Just (Positive 5)
--
-- >>> Natural 0 ^? _Positive
-- Nothing
instance AsPositive Natural where
  _Positive =
    prism'
      (\(Positive n) -> Natural n)
      (\(Natural n) -> if n == 0 then Nothing else Just (Positive n))

-- | A prism for the value one.
--
-- >>> one # ()
-- Positive 1
--
-- >>> Positive 1 ^? one
-- Just ()
--
-- >>> Positive 5 ^? one
-- Nothing
one ::
  Prism'
    Positive
    ()
one =
  prism'
    (\() -> Positive 1)
    (\(Positive n) -> if n == 1 then Just () else Nothing)

-- | The positive number one.
--
-- >>> one'
-- Positive 1
one' ::
  Positive
one' =
  one # ()

-- | A prism for the successor of a positive number.
--
-- >>> successor1 # Positive 1
-- Positive 2
--
-- >>> successor1 # Positive 4
-- Positive 5
--
-- >>> Positive 5 ^? successor1
-- Just (Positive 4)
--
-- >>> Positive 1 ^? successor1
-- Nothing
successor1 ::
  Prism'
    Positive
    Positive
successor1 =
  prism'
    (\(Positive n) -> Positive (n + 1))
    (\(Positive n) -> if n == 1 then Nothing else Just (Positive (n - 1)))

-- | The successor of a positive number.
--
-- >>> successor1' (Positive 1)
-- Positive 2
--
-- >>> successor1' (Positive 4)
-- Positive 5
successor1' ::
  Positive ->
  Positive
successor1' =
  (successor1 #)

-- | An isomorphism between 'Natural' and 'Positive' by adding/subtracting one.
--
-- >>> Natural 0 ^. successorW
-- Positive 1
--
-- >>> Natural 4 ^. successorW
-- Positive 5
--
-- >>> successorW # Positive 1
-- Natural 0
--
-- >>> successorW # Positive 5
-- Natural 4
successorW ::
  Iso'
    Natural
    Positive
successorW =
  iso
    (\(Natural n) -> Positive (n + 1))
    (\(Positive n) -> Natural (n - 1))

-- | Add two positive numbers.
--
-- >>> plus1 (Positive 3) (Positive 4)
-- Positive 7
--
-- >>> plus1 (Positive 1) (Positive 1)
-- Positive 2
plus1 ::
  Positive ->
  Positive ->
  Positive
plus1 x y =
  (_Wrapped # x <> (_Wrapped # y :: SumPositive)) ^. _Wrapped

-- | Multiply two positive numbers.
--
-- >>> multiply1 (Positive 2) (Positive 2)
-- Positive 4
--
-- >>> multiply1 (Positive 3) (Positive 4)
-- Positive 12
multiply1 ::
  Positive ->
  Positive ->
  Positive
multiply1 (Positive x) (Positive y) =
  Positive (x * y)

-- | Raise a positive number to a power.
--
-- >>> power1 (Positive 2) (Positive 10)
-- Positive 1024
--
-- >>> power1 (Positive 5) (Positive 1)
-- Positive 5
power1 ::
  Positive ->
  Positive ->
  Positive
power1 (Positive x) (Positive y) =
  Positive (x ^ y)

-- | Convert to 'Positive', defaulting to one if the value is not a valid positive.
--
-- >>> oneOr (5 :: Integer)
-- Positive 5
--
-- >>> oneOr (0 :: Integer)
-- Positive 1
oneOr ::
  (AsPositive a) =>
  a ->
  Positive
oneOr n =
  fromMaybe one' (n ^? _Positive)

-- | A prism from 'Natural' to 'Positive', succeeding when the natural is not zero.
--
-- >>> notZero # Positive 5
-- Natural 5
--
-- >>> Natural 5 ^? notZero
-- Just (Positive 5)
--
-- >>> Natural 0 ^? notZero
-- Nothing
notZero ::
  Prism'
    Natural
    Positive
notZero =
  prism'
    (\(Positive n) -> Natural n)
    (\(Natural n) -> if n == 0 then Nothing else Just (Positive n))

-- | The length of a non-empty foldable structure as a 'Positive'.
--
-- >>> length1 (1 :| [2, 3 :: Int])
-- Positive 3
--
-- >>> length1 (1 :| ([] :: [Int]))
-- Positive 1
length1 ::
  (Foldable1 f) =>
  f a ->
  Positive
length1 x =
  foldMap1 (const (SumPositive one')) x ^. _Wrapped

-- | Replicate a value a positive number of times into a 'NonEmpty'.
--
-- >>> replicate1 (Positive 3) 'x'
-- 'x' :| "xx"
--
-- >>> replicate1 (Positive 1) 'x'
-- 'x' :| ""
replicate1 ::
  Positive ->
  a ->
  NonEmpty a
replicate1 n a =
  take1 n (a :| repeat a)

-- | Take a positive number of elements from a 'NonEmpty'.
--
-- >>> take1 (Positive 2) (1 :| [2, 3 :: Int])
-- 1 :| [2]
--
-- >>> take1 (Positive 1) (1 :| [2, 3 :: Int])
-- 1 :| []
take1 ::
  Positive ->
  NonEmpty a ->
  NonEmpty a
take1 n (h :| t) =
  h :| take (successorW # n) t

-- | Drop a positive number of elements from a 'NonEmpty'.
--
-- >>> drop1 (Positive 1) (1 :| [2, 3 :: Int])
-- [2,3]
--
-- >>> drop1 (Positive 2) (1 :| [2, 3 :: Int])
-- [3]
drop1 ::
  Positive ->
  NonEmpty a ->
  [a]
drop1 n (_ :| t) =
  drop (successorW # n) t

-- | Split a 'NonEmpty' at a positive index.
--
-- >>> splitAt1 (Positive 2) (1 :| [2, 3, 4 :: Int])
-- (1 :| [2],[3,4])
--
-- >>> splitAt1 (Positive 1) (1 :| [2, 3, 4 :: Int])
-- (1 :| [],[2,3,4])
splitAt1 ::
  Positive ->
  NonEmpty a ->
  (NonEmpty a, [a])
splitAt1 n x =
  (take1 n x, drop1 n x)

-- | Safe indexing into a 'NonEmpty' by 'Positive'.
--
-- >>> (1 :| []) !!! Positive 1
-- Just 1
--
-- >>> (1 :| [2, 3 :: Int]) !!! Positive 1
-- Just 1
--
-- >>> (1 :| [2, 3 :: Int]) !!! Positive 4
-- Nothing
(!!!) ::
  NonEmpty a ->
  Positive ->
  Maybe a
(h :| t) !!! n =
  (h : t) !! (successorW # n)

-- | Find all indices where a predicate holds in a 'NonEmpty'.
--
-- >>> findIndices1 (== 'a') ('a' :| "bac")
-- [Positive 1,Positive 3]
--
-- >>> findIndices1 (== 'z') ('a' :| "bc")
-- []
findIndices1 ::
  (a -> Bool) ->
  NonEmpty a ->
  [Positive]
findIndices1 p x =
  map snd (NonEmpty.filter (p . fst) (NonEmpty.zip x (NonEmpty.iterate successor1' one')))

-- | Find the first index where a predicate holds in a 'NonEmpty'.
--
-- >>> findIndex1 (== 'a') ('b' :| "ana")
-- Just (Positive 2)
--
-- >>> findIndex1 (== 'z') ('b' :| "ana")
-- Nothing
findIndex1 ::
  (a -> Bool) ->
  NonEmpty a ->
  Maybe Positive
findIndex1 p =
  listToMaybe . findIndices1 p

-- | Find all indices of an element in a 'NonEmpty'.
--
-- >>> elemIndices1 'a' ('a' :| "bac")
-- [Positive 1,Positive 3]
--
-- >>> elemIndices1 'z' ('a' :| "bc")
-- []
elemIndices1 ::
  (Eq a) =>
  a ->
  NonEmpty a ->
  [Positive]
elemIndices1 =
  findIndices1 . (==)

-- | Find the first index of an element in a 'NonEmpty'.
--
-- >>> elemIndex1 'a' ('b' :| "ana")
-- Just (Positive 2)
--
-- >>> elemIndex1 'z' ('b' :| "ana")
-- Nothing
elemIndex1 ::
  (Eq a) =>
  a ->
  NonEmpty a ->
  Maybe Positive
elemIndex1 =
  findIndex1 . (==)

-- | Subtract two positive numbers, flooring at one.
--
-- >>> minus1 (Positive 5) (Positive 3)
-- Positive 2
--
-- >>> minus1 (Positive 3) (Positive 5)
-- Positive 1
minus1 ::
  Positive ->
  Positive ->
  Positive
minus1 (Positive x) (Positive y) =
  Positive (if x <= y then 1 else x - y)

-- | An isomorphism between 'Positive' and @'NonEmpty' ()@.
--
-- >>> Positive 3 ^. list1
-- () :| [(),()]
--
-- >>> Positive 1 ^. list1
-- () :| []
--
-- >>> list1 # (() :| [()])
-- Positive 2
list1 ::
  Iso'
    Positive
    (NonEmpty ())
list1 =
  iso
    (`replicate1` ())
    length1

-- | Convert a 'Natural' to its successor 'Positive'.
--
-- >>> plusone (Natural 0)
-- Positive 1
--
-- >>> plusone (Natural 4)
-- Positive 5
plusone ::
  Natural ->
  Positive
plusone =
  (^. successorW)

-- | Convert a 'Positive' to its predecessor 'Natural'.
--
-- >>> minusone (Positive 1)
-- Natural 0
--
-- >>> minusone (Positive 5)
-- Natural 4
minusone ::
  Positive ->
  Natural
minusone =
  (successorW #)
