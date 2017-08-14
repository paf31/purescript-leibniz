-- | This module defines a data type for _Leibniz equality_.
module Data.Leibniz
  ( Leibniz(..)
  , type (~)
  , runLeibniz
  , coerce
  , coerceSymm
  , symm
  , liftLeibniz
  , liftLeibniz1of2
  , liftLeibniz2of2
  , liftLeibniz1of3
  , liftLeibniz2of3
  , liftLeibniz3of3
  , lowerLeibniz
  , lowerLeibniz1of2
  , lowerLeibniz2of2
  , lowerLeibniz1of3
  , lowerLeibniz2of3
  , lowerLeibniz3of3
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
  compose = flip trans

instance categoryLeibniz :: Category Leibniz where
  id = refl

-- | Equality is reflexive.
refl :: forall a. a ~ a
refl = Leibniz id

-- | Equality is transitive.
trans :: forall a b c. a ~ b -> b ~ c -> a ~ c
trans (Leibniz f) (Leibniz g) = Leibniz (g <<< f)

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
liftLeibniz1of2 :: forall f a b c. a ~ b -> f a c ~ f b c
liftLeibniz1of2 _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz2of2 :: forall f a b c. a ~ b -> f c a ~ f c b
liftLeibniz2of2 _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz1of3 :: forall f a b c d. a ~ b -> f a c d ~ f b c d
liftLeibniz1of3 _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz2of3 :: forall f a b c d. a ~ b -> f c a d ~ f c b d
liftLeibniz2of3 _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz3of3 :: forall f a b c d. a ~ b -> f c d a ~ f c d b
liftLeibniz3of3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz :: forall f a b. f a ~ f b -> a ~ b
lowerLeibniz _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz1of2 :: forall f a b c d. f a c ~ f b d -> a ~ b
lowerLeibniz1of2 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2of2 :: forall f a b c d. f a c ~ f b d -> c ~ d
lowerLeibniz2of2 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz1of3 :: forall f a b c d e g. f a b c ~ f d e g -> a ~ d
lowerLeibniz1of3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2of3 :: forall f a b c d e g. f a b c ~ f d e g -> b ~ e
lowerLeibniz2of3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz3of3 :: forall f a b c d e g. f a b c ~ f d e g -> c ~ g
lowerLeibniz3of3 _ = Leibniz unsafeCoerce
