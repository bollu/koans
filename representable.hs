-- https://gist.githubusercontent.com/bollu/8224c1f4911a08595ae2c4f7769016c7/raw/b887f63aabef77286c2b934836c246725c7d690a/representable.hs

Today, we're going to be taking a look at [representable functors](https://en.wikipedia.org/wiki/Representable_functor) 
in Haskell. But first, some incantations:

```hs
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
```

We recall that a natural transformation between two functors is of the type:
```hs
type Nat f g = forall x. f x -> g x
```
Intuitively, a natural transformation between two functors is a function between functors.
for any `f x`, we have a recipe on how to convert it into `g x`. An example is
the natural transformation from `Maybe` to` List`:

```
-- maybe2list :: Nat Maybe List
maybe2list ::  forall a. Maybe a -> List a
maybe2list Nothing = []
maybe2list (Just a) = [a]
```

Also recall that the function type `a -> b` is also called as `Hom(a, b)` or `Hom a b`.
To stay consistent with category theory terminology, we're going to create a type 
synonym:

```hs
type Hom a b = a -> b
```

Now, a functor `f` is saif to be _representable_ if it is the same  to `Hom a` for some
`a`. Let's meditate on this a little bit. So, we have:

1. A functor `f`
2. An object `a`, such that `f` is the same as `Hom a`
3. A way to convert `f` into `Hom a`. That is, we have a natural transformation from `f` to `Hom a`.
   This is the same as asking for `Nat f (Hom a)`, or `forall x. f x -> Hom a x`, which is the same
   as `forall x. f x -> (a -> x)`.
4. We also want a way to convert `Hom a` into `f`, since `f` is "the same" as `Hom a`. This asks for
   a `Nat (Hom a) f`, or a `forall x. Hom a x -> f x` or `forall x. (a -> x) -> f x`.


Intuitively, this is telling us that there's some object `a` such that `f x` is always equal
to `a -> x` for any choice of `x`. So we get an equivalence beween some "data" `f x`, and
a function `a -> x`. How do we encode this in haskell? What's an example? Let's see!

##### Haskell encoding

```hs
-- Functor f is representable iff isomorphic to SOME hom functor
-- f: C -> Set -- set to be isomorphic to hom functor
-- ∃d∈C, f ~= Hom(d, -)
-- d is called as the "representing object" of functor f
class Functor f => Representable f where
    type family RepresentingObj f :: *
    -- fwd :: Nat f (Hom d)
    -- fwd :: forall x. f x ->  (Hom d) x
    fwd :: forall x. f x ->  (RepresentingObj f -> x) 
    bwd :: forall x. (RepresentingObj f -> x) -> f x -- memoization
```

The idea is encoded as shown above, where we say that a functor `f` is `Representable`,
if (1) it has a `RepresentingObj f` which represents it, and (2) there are functions
to convert `f x` into `(RepresentingObj f -> x)`.

##### Prototypical example: Streams

Consider the data type of infinite streams:
```hs
-- | always infinite stream
data Stream a = SCons a (Stream a)
```

for example, the infinite stream of zeros is an inhabitant of this:

```hs
zeros :: Stream Int
zeros = SCons 0 zeros
```

as is the stream of all natural numbers:

```hs
nats :: Stream Int
nats = let go n = SCons n (go (n+1)) in go 0
```


This is a functor, similar to how list is a functor:

```hs
instance Functor Stream where
    fmap f (SCons x xs) = SCons (f x) (fmap f xs)
```

More interestingly, it is a *representable functor*,
since we can think of a `Stream a` as a function `(Integer -> a)`,
as we can index a stream at an arbitrary integer. Similarly, given
an `(Integer -> a)`, we can build a `Stream a` from it by [memoizing](https://en.wikipedia.org/wiki/Memoization)
the function `(Integer -> a)` into the data structure `Stream a`:

```hs
instance Representable Stream where
    type RepresentingObj Stream = Integer
    -- fwd:: Stream a -> (Integer -> a)
    fwd (SCons x _) 0 = x
    fwd (SCons _ xs) n = fwd xs (n-1)

    -- bwd:: (Integer -> a) -> Stream a
    bwd int2x = go 0 where
        go n = SCons (int2x n) (go (n+1))
```

Clearly, these are invertible, and thus we witness the equality between `Stream a`
and `Integer -> a`


##### Non-example: Lists

Lists are an example of a type that is *not* a representable functor, 
because different lists can contain a different numbers of elements. so,
for example, we can't use `Integer` as the representing object (ie, we can't
write all lists as functions `Natural -> a`, because the empty list
contains *no* elements, so it would have to be a function `Empty -> a`,
while the list containing one element has a *single* element, so it
would be the function `() -> a`, the list containing two elements
has *two* elements, which would be the function `Bool -> a`, and so on.

Thus, it's impossible for us to find a representing object for a list!


##### Crazy example: Vectors

Since we saw that lists aren't representable, because we can query
"outside their domain", the question naturally becomes, can we build a data
structure such that the query remains within the domain? The answer is yes,
with much trickery!

First, we'll need an encoding of natural numbers as `0` and a successor
function (intuitively, `+1`), where we represent a number `n` as adding `1`
`n` times to `0`:

```hs
data NAT = ZERO | SUCC NAT deriving(Eq)
```

so, zero will be `ZERO`, one will be `SUCC ZERO`, two will be `SUCC (SUCC ZERO)`,
and so on. We implement `countNAT` to convert naturals to integers:

```hs
countNAT :: NAT -> Int
countNAT ZERO = 0
countNAT (SUCC n) = countNAT n + 1
instance Show NAT where show n = show (countNAT n)
```

Next, we create a type `FinSet (n :: NAT) :: *`, where `FinSet n`
contains EXACTLY `n` members. Yes, that statement has a lot to unpack:

The type `FinSet n` has `n :: NAT`. So we can ask for `FinSet ZERO`, `FinSet
(SUCC ZERO)`, `FinSet (SUCC (SUCC ZERO))` and so on. Notice that in reality,
`ZERO`, `SUCC ZERO` and so on are *values* in Haskell. However, we are
creating a type `FinSet n` which depends on *values*. To enable this, we use
the `DataKinds` extension. Such types which can dependent on values fall under
a class of type systems known as [dependent types](https://en.wikipedia.org/wiki/Dependent_type).

Let's first see the definition, and then try and gain some intuition of how this works:

```hs
data FinSet (n :: NAT) where
    Intros :: FinSet (SUCC n)
    Lift :: FinSet n -> FinSet (SUCC n)
```

Why does this work? I claim that `FinSet n` only has `n` numbers. Well, let's see.
Can I ever crete a member of `FinSet ZERO`? No, both the constructors `Intros`
and `Lift` create `FinSet (Succ _)` for some `_`, so it's impossible to get my hands
on a `FinSet ZERO`. What about `FinSet (SUCC ZERO)` [aka `FinSet 1`]? Yes, there's
one way to get such an element:

```
Intros :: FinSet (SUCC ZERO)
```

and that's the only way to get a member of `FinSet (SUCC ZERO)`!
What about `FinSet (SUCC (SUCC ZERO))`? We have two inhabitants:

```
FinSet (SUCC (SUCC ZERO))
- Intros :: FinSet (SUCC (SUCC ZERO))
- Lift (Intros :: FinSet (SUCC ZERO)) :: FinSet (SUCC (SUCC ZERO))
```

and so on. Here's the `Show` instance for a `FinSet`, where we think of `Intros`
as `1`, and `Lift` as incrementing the number.


```hs
countFinSet :: FinSet n -> Int
countFinSet Intros = 1
countFinSet (Lift x) = 1 + countFinSet x

instance Show (FinSet n) where
    show x = show (countFinSet x)
```

We can use the same mechanism to encode `Vector n a`, a vector
with exactly `n` elements!


```hs
data Vector (n :: NAT) (a :: *)  where
    VNil :: Vector ZERO a
    VCons :: a -> Vector n a -> Vector (SUCC n) a

instance Functor (Vector n) where
    fmap _ VNil = VNil
    fmap f (VCons a as) = VCons (f a) (fmap f as)
```




```hs
instance Representable (Vector n) where
    type RepresentingObj (Vector n) = FinSet n

    -- fwd :: forall x. f x ->  (RepresentingObj f -> x) 
    fwd :: forall x. Vector n x -> ((FinSet n) -> x)
    fwd (VCons x xs) Intros = x
    fwd (VCons x xs) (Lift n) = fwd xs n

    bwd :: forall x. ((FinSet n) -> x) -> Vector n x
    bwd finset2x = undefined
```

Unfortunately, I couldn't quite figure out how to write `bwd` in Haskell, thus
I leave it as an exercise for the reader :) 
