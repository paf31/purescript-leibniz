-- | This module defines a data type for _Leibniz equality_.
module Data.Leibniz
  ( Leibniz(..)
  , type (~)
  , runLeibniz
  , coerce
  , coerceSymm
  , symm
  , liftLeibniz
  , liftLeibniz2
  , liftLeibniz3
  , lowerLeibniz
  , lowerLeibniz2
  , lowerLeibniz3
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

-- | Two types are equal if they are _equal in all contexts_.
newtype Leibniz a b = Leibniz (forall f. f a -> f b)

infix 4 type Leibniz as ~

-- | Unpack a Leibniz equality.
runLeibniz :: forall f a b. a ~ b -> f a -> f b
runLeibniz (Leibniz f) = f

instance semigroupoidLeibniz :: Semigroupoid Leibniz where
  compose _ _ = Leibniz unsafeCoerce

instance categoryLeibniz :: Category Leibniz where
  id = Leibniz id

-- | Equality is symmetric.
symm :: forall a b. a ~ b -> b ~ a
symm _ = Leibniz unsafeCoerce

-- | Coerce a value of type `a` to a value of the Leibniz-equal type `b`.
coerce :: forall a b. a ~ b -> a -> b
coerce _ = unsafeCoerce

-- | Coerce a value of type `b` to a value of the Leibniz-equal type `a`.
coerceSymm :: forall a b. a ~ b -> b -> a
coerceSymm _ = unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz :: forall f a b. a ~ b -> f a ~ f b
liftLeibniz _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz2 :: forall f a b c. a ~ b -> f a c ~ f b c
liftLeibniz2 _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz3 :: forall f a b c d. a ~ b -> f a c d ~ f b c d
liftLeibniz3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz :: forall f a b. f a ~ f b -> a ~ b
lowerLeibniz _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2 :: forall f a b c d. f a c ~ f b d -> a ~ b
lowerLeibniz2 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2' :: forall f a b c d. f a c ~ f b d -> c ~ d
lowerLeibniz2' _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz3 :: forall f a b c d e g. f a b c ~ f d e g -> a ~ d
lowerLeibniz3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz3' :: forall f a b c d e g. f a b c ~ f d e g -> b ~ e
lowerLeibniz3' _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz3'' :: forall f a b c d e g. f a b c ~ f d e g -> c ~ g
lowerLeibniz3'' _ = Leibniz unsafeCoerce
