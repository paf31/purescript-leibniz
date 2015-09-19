## Module Data.Leibniz

This module defines a data type for _Leibniz equality_.

#### `Leibniz`

``` purescript
newtype Leibniz a b
  = Leibniz (forall f. f a -> f b)
```

Two types are equal if they are _equal in all contexts_.

##### Instances
``` purescript
instance semigroupoidLeibniz :: Semigroupoid Leibniz
instance categoryLeibniz :: Category Leibniz
```

#### `runLeibniz`

``` purescript
runLeibniz :: forall f a b. Leibniz a b -> f a -> f b
```

Unpack a Leibniz equality.

#### `symm`

``` purescript
symm :: forall a b. Leibniz a b -> Leibniz b a
```

Equality is symmetric.

#### `coerce`

``` purescript
coerce :: forall f a b. Leibniz a b -> a -> b
```

Coerce a value of type `a` to a value of the Leibniz-equal type `b`

#### `liftLeibniz`

``` purescript
liftLeibniz :: forall f a b. Leibniz a b -> Leibniz (f a) (f b)
```

Lift equality over a type constructor.

#### `liftLeibniz2`

``` purescript
liftLeibniz2 :: forall f a b c. Leibniz a b -> Leibniz (f a c) (f b c)
```

Lift equality over a type constructor.

#### `liftLeibniz3`

``` purescript
liftLeibniz3 :: forall f a b c d. Leibniz a b -> Leibniz (f a c d) (f b c d)
```

Lift equality over a type constructor.

#### `lowerLeibniz`

``` purescript
lowerLeibniz :: forall f a b. Leibniz (f a) (f b) -> Leibniz a b
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz2`

``` purescript
lowerLeibniz2 :: forall f a b c. Leibniz (f a c) (f b c) -> Leibniz a b
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz3`

``` purescript
lowerLeibniz3 :: forall f a b c d. Leibniz (f a c d) (f b c d) -> Leibniz a b
```

Every type constructor in PureScript is injective.


