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
Semigroupoid Leibniz
Category Leibniz
```

#### `type (~)`

``` purescript
infix 4 type Leibniz as ype (~
```

#### `runLeibniz`

``` purescript
runLeibniz :: forall f a b. a ~ b -> f a -> f b
```

Unpack a Leibniz equality.

#### `symm`

``` purescript
symm :: forall a b. a ~ b -> b ~ a
```

Equality is symmetric.

#### `coerce`

``` purescript
coerce :: forall a b. a ~ b -> a -> b
```

Coerce a value of type `a` to a value of the Leibniz-equal type `b`

#### `liftLeibniz`

``` purescript
liftLeibniz :: forall f a b. a ~ b -> (f a) ~ (f b)
```

Lift equality over a type constructor.

#### `liftLeibniz2`

``` purescript
liftLeibniz2 :: forall f a b c. a ~ b -> (f a c) ~ (f b c)
```

Lift equality over a type constructor.

#### `liftLeibniz3`

``` purescript
liftLeibniz3 :: forall f a b c d. a ~ b -> (f a c d) ~ (f b c d)
```

Lift equality over a type constructor.

#### `lowerLeibniz`

``` purescript
lowerLeibniz :: forall f a b. (f a) ~ (f b) -> a ~ b
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz2`

``` purescript
lowerLeibniz2 :: forall f a b c. (f a c) ~ (f b c) -> a ~ b
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz3`

``` purescript
lowerLeibniz3 :: forall f a b c d. (f a c d) ~ (f b c d) -> a ~ b
```

Every type constructor in PureScript is injective.


