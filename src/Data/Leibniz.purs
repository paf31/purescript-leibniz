-- | This module defines a data type for _Leibniz equality_.
module Data.Leibniz 
  ( Leibniz(..)
  , runLeibniz
  , coerce
  , symm
  , liftLeibniz
  , liftLeibniz2
  , liftLeibniz3
  , lowerLeibniz
  , lowerLeibniz2
  , lowerLeibniz3
  ) where
    
import Prelude

import Unsafe.Coerce
    
-- | Two types are equal if they are _equal in all contexts_.
newtype Leibniz a b = Leibniz (forall f. f a -> f b)

-- | Unpack a Leibniz equality.
runLeibniz :: forall f a b. Leibniz a b -> f a -> f b
runLeibniz (Leibniz f) = f

instance semigroupoidLeibniz :: Semigroupoid Leibniz where
  compose _ _ = Leibniz unsafeCoerce

instance categoryLeibniz :: Category Leibniz where
  id = Leibniz id
  
-- | Equality is symmetric.
symm :: forall a b. Leibniz a b -> Leibniz b a
symm _ = Leibniz unsafeCoerce

-- | Coerce a value of type `a` to a value of the Leibniz-equal type `b`
coerce :: forall f a b. Leibniz a b -> a -> b
coerce _ = unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz :: forall f a b. Leibniz a b -> Leibniz (f a) (f b)
liftLeibniz _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz2 :: forall f a b c. Leibniz a b -> Leibniz (f a c) (f b c)
liftLeibniz2 _ = Leibniz unsafeCoerce

-- | Lift equality over a type constructor.
liftLeibniz3 :: forall f a b c d. Leibniz a b -> Leibniz (f a c d) (f b c d)
liftLeibniz3 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz :: forall f a b. Leibniz (f a) (f b) -> Leibniz a b
lowerLeibniz _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz2 :: forall f a b c. Leibniz (f a c) (f b c) -> Leibniz a b
lowerLeibniz2 _ = Leibniz unsafeCoerce

-- | Every type constructor in PureScript is injective.
lowerLeibniz3 :: forall f a b c d. Leibniz (f a c d) (f b c d) -> Leibniz a b
lowerLeibniz3 _ = Leibniz unsafeCoerce