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

Coerce a value of type `a` to a value of the Leibniz-equal type `b`.

#### `coerceSymm`

``` purescript
coerceSymm :: forall a b. a ~ b -> b -> a
```

Coerce a value of type `b` to a value of the Leibniz-equal type `a`.

#### `liftLeibniz`

``` purescript
liftLeibniz :: forall f a b. a ~ b -> (f a) ~ (f b)
```

Lift equality over a type constructor.

#### `liftLeibniz1of2`

``` purescript
liftLeibniz1of2 :: forall f a b c. a ~ b -> (f a c) ~ (f b c)
```

Lift equality over a type constructor.

#### `liftLeibniz2of2`

``` purescript
liftLeibniz2of2 :: forall f a b c. a ~ b -> (f c a) ~ (f c b)
```

Lift equality over a type constructor.

#### `liftLeibniz1of3`

``` purescript
liftLeibniz1of3 :: forall f a b c d. a ~ b -> (f a c d) ~ (f b c d)
```

Lift equality over a type constructor.

#### `liftLeibniz2of3`

``` purescript
liftLeibniz2of3 :: forall f a b c d. a ~ b -> (f c a d) ~ (f c b d)
```

Lift equality over a type constructor.

#### `liftLeibniz3of3`

``` purescript
liftLeibniz3of3 :: forall f a b c d. a ~ b -> (f c d a) ~ (f c d b)
```

Lift equality over a type constructor.

#### `lowerLeibniz`

``` purescript
lowerLeibniz :: forall f a b. (f a) ~ (f b) -> a ~ b
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz1of2`

``` purescript
lowerLeibniz1of2 :: forall f a b c d. (f a c) ~ (f b d) -> a ~ b
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz2of2`

``` purescript
lowerLeibniz2of2 :: forall f a b c d. (f a c) ~ (f b d) -> c ~ d
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz1of3`

``` purescript
lowerLeibniz1of3 :: forall f a b c d e g. (f a b c) ~ (f d e g) -> a ~ d
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz2of3`

``` purescript
lowerLeibniz2of3 :: forall f a b c d e g. (f a b c) ~ (f d e g) -> b ~ e
```

Every type constructor in PureScript is injective.

#### `lowerLeibniz3of3`

``` purescript
lowerLeibniz3of3 :: forall f a b c d e g. (f a b c) ~ (f d e g) -> c ~ g
```

Every type constructor in PureScript is injective.


