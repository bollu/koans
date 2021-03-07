Through the looking class: Contravariant functors and applicatives
==================================================================

In this blog post, we will learn about `Contravariant` and 
`Divisible` which provide duals for `Data.Functor` and `Data.Applicative`
respectively.

> {-# LANGUAGE NoImplicitPrelude #-}
> import GHC.Base (IO, return)
> import Data.Functor.Contravariant.Divisible 
> import Data.Functor.Contravariant


Dual Functors
-------------

First, a quick recap of functors:

> class Functor f where
>    fmap :: (a -> b) -> f a -> f b

This lets us lift a function `f: a -> b` into a `fmap f: f a -> f b`.
The dual is a type:


> class Contravariant f where
>   contramap :: (a -> b) -> f b -> f a

Let's look at some example to build our intuition of such a typeclass.

Predicates
----------

The classic example is that of a *predicate*, which is something that
tells us whether a value of type `t` obeys some property or not:

> data Predicate t = Predicate { runPredicate :: t -> Bool }

> instance Contravariant Predicate where
>   contramap a2b b2bool = Predicate (\a -> b2bool (a2b (a)))

An example of such a thing is if we know how to check a
real number is greater than zero:

> reGt0 :: Predicate Float
> reGt0 x = Preciate (x > 0)

and we can converts integers into reals:

> intToReal :: Int -> Float
> intToReal i = fromInteger 


then we can check if an integer is greater than zero:

> intGt0 :: Predicate Int
> intGt0 = contramap intToReal reGt0

This is described by the picture:

<img src='real-to-z-contravariance.png'/>

So, such a `Predicate Float` "consumes" a `Float` to produce a `Bool`.
we can "pull back" the consumption along a function `Int -> Float` to consume
a `Int` and produce a `Bool`.

Dual Applicatives
-----------------


> class Functor f => Applicative f where
>    pure :: a -> f a
>    (<*>) :: f (a -> b) -> f a -> f b

Recall that an `Applicative` allow us to work with
tuples:

```
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

We can write the type of liftA2 to be more suggestive as:
```
liftA2 :: ((a, b) -> c) -> ((f a, f b) -> f c)
```

If we can **combine** a tuple `(a, b)` into a value `c`,
then we can glue lifted values `(f a, f b)` into a lifted `f c`.


The dual, called `Divisible`, says that if we can **break** a value `c` into `(a, b)`,
then we can glue lifted values `(f a, f b)` into a lifted `f c`.

> class Contravariant f => Divisible f where
>   divide :: (c -> (a, b)) -> f a -> f b -> f c 
>   conquer :: f a

<img src='divisible-consumer.png' />


Example 1: Predicates
---------------------

We can continue our example of predicates. If we know how to check if
something holds for `a` and something holds for `b`, we can check how something
holds for `(a, b)`: check for **both** `a` **and** `b`. So, this would be:

> instance Divisible Predicate where
>   divide :: (c -> (a, b)) -> 
>             Predicate a -> Predicate b -> Predicate c
>   divide c2ab a2bool b2bool = 
>     Predicate (\c -> let (a, b) = c2ab c 
>                      in a2bool a && b2bool b)
>   conquer :: Predicate a
>   conquer = Predicate (\a -> True)

Example 2: Serialization
------------------------

Consider the ability to convert a data type to a string. These "consume" the (varying)
data types to produce a `String`. So, for example:

> data Serializer a = Serializer { serialize :: a -> String }

> instance Contravariant Serializer where
>   contramap :: (a -> b) -> Serializer b -> Serializer a
>   contramap a2b b2string = Serializer (\a -> b2string (a2b a))

> instance Divisible Serializer where
>   divide :: (c -> (a, b))
>             -> Serializer a
>             -> Serializer b
>             -> Serializer c
>   divide 
>   conquer :: Serializer String
>   conquer = Serializer (\a -> "")


Example 3 / Generalization: Fixed output type
---------------------------------

Indeed, we can generalize both examples: `Predicate` is all functions into a fixed output
type `Bool`, while a serializer is functions into a fixed output type `String`. We need
to know how to combine the outputs --- in the case of `Bool`, we combined the outputs
with `&&`. In the case of `String`, we combined the outputs with `<>`. In general,
we need a **monoid**.

> data Into y x = Into { runInto :: x -> y }
> instance Contravariant (Into y) where
>    contramap :: (b -> a)
>                 -> Into a -- a -> y
>                 -> Into b -- b -> y
>    contramap b2a a2y = Into (\b -> a2y (b2a b))                   

> instance Monoid y => Divisible (Into y) where
>    divide :: (c -> (a, b))
>              -> Into a -- a -> y
>              -> Into b -- b -> y
>              -> Into c -- c -> y
>    divide c2ab a2y b2y = 
>      Into (\c -> let (a, b) = c2ab c 
>                  in (a2y a) <> (b2y b))
>    conquer :: Into a -- a -> y
>    conquer = Into (\a -> mempty)


In all of these examples, we have (a) A data structure that can be _decomposed_: this is
the part of `c -> (a, b)`, and (b) A consumer of data: `f a` is "something that can consume an `a`.

The laws for Contravariant
--------------------------

So far, I've been skating on intuition, without telling you what the *laws* `Divisible` must follow
are. Let's get formal now. For a given `Contravariant f`, we need an `fmap`-like law to hold:

- `fmap`'s law:: `fmap (f . g) = fmap f . fmap g
- `contramap`'s law: `contramap (f . g) = contramap g . contramap f`

See that the order gets flipped, in comparison to `fmap`.
Let's check that this law holds for `Into y`, since that was the most general
example.

```
contramap :: (p -> q) -> Into y q -> Into y p
x2q :: x -> q
contramap (x2q . p2x) $ (Into q2y) =? contramap p2x . conramap x2q $ (Into q2y)
```

We can rewrite our `Into y` definition to be easier to manipulate using
[point-free style](https://wiki.haskell.org/Pointfree):

```hs
instance Contravariant Into where

>    contramap :: (b -> a)
>                 -> Into a -- a -> y
>                 -> Into b -- b -> y
>    contramap b2a (Into a2y) = Into (a2y . b2a) -- b -> y
```

if we now try to simplify:

1. `contramap p2x . contramap x2q $ (Into q2y)`
2. Remove `.` and `$`: `contramap p2x (contramap x2q (Into q2y))`
3. unwrap inner `contramap`: `contramap p2x (Into (q2y . x2q))
3. unwrap outer `contramap`: `Into (q2y . x2q . p2x)`
3. re-group `.`: `contramap`: `Into (q2y . (x2q . p2x))`
4. introduce back `contramap`: `contramap (x2q . p2x) (Into q2y)`

and thus we're done! We've shown that the `Contravariant` laws hold for `Into`

The laws for Divisible
----------------------

The laws follow from some [category theory](https://hackage.haskell.org/package/contravariant-1.5.3/docs/Data-Functor-Contravariant-Divisible.html#g:3). We need that for the function:

> delta :: a -> (a, a)
> delta a = (a, a)

the following relations between `divide` and `conquer` hold:

0. First, let's think about `divide delta`. This means that we
   perform the *same* action on the left element and right element of
   the tuple, because our tuple is built from the same element `a`.


> dd :: Divisible f => (f a, f a) -> f a
> dd = divide  delta

1. `conquer` is a identity element for `divide delta`:

```hs
dd m conquer = dd conquer m
```

2. `divide delta` is associative: 

```hs
dd m (dd n o) = dd (dd m n) o
```

So this is saying that `divide delta` is some kind of monoid, with `conquer`
as the identity element. Let's verify what happens in our case of `Into y`.

```hs
dd 
```

> main = return ()


- [Edward Kmett's talk](https://www.youtube.com/watch?v=cB8DapKQz-I)
