{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

We begin by considering the idea of continuations:

```
data Cont a = Cont { useCont :: forall r. (a -> r) -> r }
```

Recall that the idea of a continuation is that 
we create a _Promise_ of a value `a`. This value
can be extracted from a `Cont a` by providing a continuation
handler of type `auser : a -> r` for _any_ return type `r`.
An example usage is below:

```hs
mkCont :: a -> Cont a
mkCont a = Cont $ \auser -> a user a

k3 :: Cont Int; k3 = mkCont 3

intUser :: Int -> String

user :: String;
user = useCont k3 $ \i -> 
  case i of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    _ -> "unk"

main :: IO (); main = print used
```

Continuations are generally named with a `k` for `K`ontinuation. In the above
example, the continuation `mkCont 3` is named as `k3` for a continuation holding
a value of `3`. We build this cotinuation using `mkCont 3`. This continuation
is used by the value `user`, which uses the continuation `k3 :: Cont Int` by
calling `useCont`. We then provide a handler `\i -> ...` which tells `useCont` how
we wish to handle the value `i`. We case-analyze on `i`, and print the appropriate 
english version of the value `i`. The program will print out `3`, since we extracted
the value from `k3 = mkCont 3`, a continuation that held the value `3`.


Is there some way we can recover the value the value `3` that was used to _build_ the
continuation `k3 = mkCont 3`? There is! If we say:

```hs
extract :: Int
extract = useCont k3 (\i -> i)
```

we are saying that we want to use the `i`

We will now consider a generalization of the above, given by the mysterious type

```hs
data ContF f a = ContF { useContF :: forall r. (a -> r) -> f r }
```

where when we provide a user of `a` of type `(a -> r)`, the continuation
produces a value of type `f r`. What value does this provide? Well, first off,
if we set `f = Id`, we recover the original continuations:

```hs
-- vv--Continuation monad--vv
-- F = id .
type Id x = x
-- Nat(Hom(a, -), Id - ) ~= Id a
-- Nat(forall x. (a -> x), Id x) ~= Id a
-- Nat(forall x. (a -> x) -> x) ~= a
idyo :: (forall x. (a -> x) -> x) -> a
idyo k = k id

idyo' ::  a -> (forall x. (a -> x) -> x)
idyo' a = \k -> k a
```


Can we do more? Let's try picking `f = []` and see what happens: the type
looks like:

```
ContF [] a = forall r. (a -> r) -> [r]
```

So, given a thing that uses an `a`, a `(a -> r)`, we produce a list of rs, `[r]`.
Since we're in a pure functional language, intuitively, the only way such a thing is possible
is if we call the function `(a -> r)` .


-- Encoding the Yoneda lemma in Haskell

-- in Hask, we conflate the arrow in Hom(X, -) with the arrow in Nat(., .)
-- g ~= Nat(Hom(a, -), g)
-- RHS = Nat(Hom(a, -), g) = Yonda.
-- take components at y, we get RHS y:
--     Nat(Hom(a, y), g y)
type Yoneda g a = (forall y. (a -> y) -> g y)

lemma_toyoneda :: Functor g => g x -> Yoneda g x
lemma_toyoneda  g_x yoneda = fmap yoneda  g_x


lemma_fromyoneda :: Functor g => Yoneda g x -> g x
lemma_fromyoneda yoneda = yoneda id

-- pick x = a
-- Nat(Hom(a, -), F -) ~= F a
type Hom a x = (a -> x)
type List a = [a]
type Nat f g = forall x. f x -> g x
listyo :: (Nat (Hom a) List) -> [a]
-- listyo :: (forall x. (Hom a x) -> List x) -> [a]
-- listyo :: (f: forall x. (g: a -> x) -> [x]) -> [a]
-- implement f, is to use g multiple times
-- f g = [g a1, g a2, ... g an] = fmap g [a1, a2, ... an]
-- f id = [id a1, id a2, ... id an] = [a1, a2, ... an]
listyo f = f id

-- F = [.]
listyo' ::  [a] -> (forall x. (a-> x) -> [x])
listyo' as = \f -> fmap f as

-- the f will be like f g = [g a1, g a2, ... g an] = fmap g [a1, ..., an]


-- F = (b -> .)
-- pick x = a
-- ((a -> a) -> (b -> a) -> (b -> a)
-- Nat(Hom(a, -), F) ~= F a
-- type Hom a b = (a -> b)
-- c -> Hom (c, -)
-- a, b. arrows ∈ Hom (a, b)
-- Hom (a, -), Hom(b, -). arrows ∈ Nat Hom (a, -) Hom(b, -)
--
-- a, b. arrows ∈ Hom (a, b)
-- Hom (-, a), Hom(-, b). arrows ∈ Nat Hom (-, a) Hom(-, b)
contrayo :: Nat (Hom a) (Hom b) -> Hom b a
contrayo :: (forall x. (a -> x) -> (b -> x)) -> (b -> a)
contrayo f = f id

contrayo' ::   (b -> a) -> (forall x. (a -> x) -> (b -> x))
contrayo' b2a = \a2x -> a2x . b2a

