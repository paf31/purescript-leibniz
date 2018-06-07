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
  , applyLeibniz
  , applyLeibniz1of2
  , applyLeibniz2of2
  , applyLeibniz1of3
  , applyLeibniz2of3
  , applyLeibniz3of3
  , lowerLeibniz
  , lowerLeibniz1of2
  , lowerLeibniz2of2
  , lowerLeibniz1of3
  , lowerLeibniz2of3
  , lowerLeibniz3of3
  , class Distinguish
  , refute
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
  identity = refl

-- | Equality is reflexive.
refl :: forall a. a ~ a
refl = Leibniz identity

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
liftLeibniz = applyLeibniz identity

-- | Lift equality over a type constructor.
liftLeibniz1of2 :: forall f a b c. a ~ b -> f a c ~ f b c
liftLeibniz1of2 = applyLeibniz1of2 identity

-- | Lift equality over a type constructor.
liftLeibniz2of2 :: forall f a b c. a ~ b -> f c a ~ f c b
liftLeibniz2of2 = applyLeibniz2of2 identity

-- | Lift equality over a type constructor.
liftLeibniz1of3 :: forall f a b c d. a ~ b -> f a c d ~ f b c d
liftLeibniz1of3 = applyLeibniz1of3 identity

-- | Lift equality over a type constructor.
liftLeibniz2of3 :: forall f a b c d. a ~ b -> f c a d ~ f c b d
liftLeibniz2of3 = applyLeibniz2of3 identity

-- | Lift equality over a type constructor.
liftLeibniz3of3 :: forall f a b c d. a ~ b -> f c d a ~ f c d b
liftLeibniz3of3 = applyLeibniz3of3 identity

-- | Apply an equality of type constructors to an equality of types.
applyLeibniz :: forall f g a b c d. f c ~ g d -> a ~ b -> f a ~ g b
applyLeibniz _ _ = Leibniz unsafeCoerce

-- | Apply an equality of type constructors to an equality of types.
applyLeibniz1of2 :: forall f g f1 f2 g1 g2 a b c. f f1 f2 ~ g g1 g2 -> a ~ b -> f a c ~ g b c
applyLeibniz1of2 _ _ = Leibniz unsafeCoerce

-- | Apply an equality of type constructors to an equality of types.
applyLeibniz2of2 :: forall f g f1 f2 g1 g2 a b c. f f1 f2 ~ g g1 g2 -> a ~ b -> f c a ~ g c b
applyLeibniz2of2 _ _ = Leibniz unsafeCoerce

-- | Apply an equality of type constructors to an equality of types.
applyLeibniz1of3 :: forall f g f1 f2 f3 g1 g2 g3 a b c d. f f1 f2 f3 ~ g g1 g2 g3 -> a ~ b -> f a c d ~ g b c d
applyLeibniz1of3 _ _ = Leibniz unsafeCoerce

-- | Apply an equality of type constructors to an equality of types.
applyLeibniz2of3 :: forall f g f1 f2 f3 g1 g2 g3 a b c d. f f1 f2 f3 ~ g g1 g2 g3 -> a ~ b -> f c a d ~ g c b d
applyLeibniz2of3 _ _ = Leibniz unsafeCoerce

-- | Apply an equality of type constructors to an equality of types.
applyLeibniz3of3 :: forall f g f1 f2 f3 g1 g2 g3 a b c d. f f1 f2 f3 ~ g g1 g2 g3 -> a ~ b -> f c d a ~ g c d b
applyLeibniz3of3 _ _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz :: forall f g a b. f a ~ g b -> a ~ b
lowerLeibniz _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz1of2 :: forall f g a b c d. f a c ~ g b d -> a ~ b
lowerLeibniz1of2 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2of2 :: forall f g a b c d. f a c ~ g b d -> c ~ d
lowerLeibniz2of2 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz1of3 :: forall f g a b c d e h. f a b c ~ g d e h -> a ~ d
lowerLeibniz1of3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2of3 :: forall f g a b c d e h. f a b c ~ g d e h -> b ~ e
lowerLeibniz2of3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz3of3 :: forall f g a b c d e h. f a b c ~ g d e h -> c ~ h
lowerLeibniz3of3 _ = Leibniz unsafeCoerce

-- | This class is used in the definition of the `refute` function.
-- |
-- | Its job is to distinguish the two types `a` and `b` (if possible)
-- | by mapping them to the different output types `Unit` and `Void`
-- | respectively.
class Distinguish a b c o | a b c -> o

instance distinguishLeft :: Distinguish a b a Unit else
instance distinguishRight :: Distinguish a b b Void

-- | A type for which `Distinguished a b c` is isomorphic to the unique output
-- | `o` of the `Distinguish` relation such that `Distinguish a b c o` holds.
foreign import data Distinguished :: Type -> Type -> Type -> Type

distinguished :: forall a b c r. Distinguish a b c r => r -> Distinguished a b c
distinguished = unsafeCoerce

unDistinguished :: forall a b c r. Distinguish a b c r => Distinguished a b c -> r
unDistinguished = unsafeCoerce

-- | Refute a type equality for two types which are not definitionally equal.
-- |
-- | For example, in the REPL:
-- |
-- | ```
-- | > import Data.Leibniz
-- |
-- | > :type \(l :: String ~ Int) -> refute l
-- | forall r. Leibniz String Int -> r
-- |
-- | > :type \(l :: String ~ String) -> refute l
-- | Error found:
-- |   Could not match type Unit with type Void
-- | ```
-- |
-- | The error message here is due to the way in which the `Distinguish` class decides
-- | apartness.
refute :: forall a b r. Distinguish a b b Void => a ~ b -> r
refute l = absurd (unDistinguished (runLeibniz l (distinguished unit :: Distinguished a b a)))
