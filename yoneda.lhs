## Yoneda as enriched continuations

In this article, we explore the Yoneda Lemma as telling us about "enriched continuations"

```hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
```


We begin by considering the idea of continuations:

```hs
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
    3 -> "three" -- this is printed
    4 -> "four"
    _ -> "unk"

-- | prints 3, since `user` uses the continuation `k3`
-- | to produce the number `3`.
main :: IO (); main = print user
```

Continuations are generally named with a `k` for `K`ontinuation. In the above
example, the continuation `mkCont 3` is named as `k3` for a continuation holding
a value of `3`. We build this continuation using `mkCont 3`. This continuation
is used by the value `user`, which uses the continuation `k3 :: Cont Int` by
calling `useCont`. We then provide a handler `\i -> ...` which tells `useCont` how
we wish to handle the value `i`. We case-analyze on `i`, and print the appropriate 
English version of the value `i`. The program will print out `3`, since we extracted
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
-- F = id .
-- idyo :: (forall x. (a -> x) -> f x) -> a
-- idyo :: (forall x. (a -> x) -> id x) -> a
idyo :: (forall x. (a -> x) -> x) -> a
idyo k = k id

-- idyo' ::  a -> (forall x. (a -> x) -> f x)
-- idyo' ::  a -> (forall x. (a -> x) -> id x)
idyo' ::  a -> (forall x. (a -> x) -> x)
idyo' a = \k -> k a
```


Can we do more? Let's try picking `f = []` and see what happens: the type
looks like:

```hs
ContF [] a ~ forall r. (a -> r) -> [r]
```

So, given a thing that uses an `a`, a `(a -> r)`, we produce a list of rs, `[r]`.
Since we're in a pure functional language, intuitively, the only way such a thing is possible
is if we call the function `(a -> r)` .


We ca show that the two sides are equal:

```
lemma_toyoneda :: Functor g => g x -> Yoneda g x
lemma_toyoneda  g_x yoneda = fmap yoneda  g_x


lemma_fromyoneda :: Functor g => Yoneda g x -> g x
lemma_fromyoneda yoneda = yoneda id
```

In general, the `Yoneda g x` will correspond to a "functorial continuation", where given a handler `(a -> r)`,
we are expected to return a `f r`. The two lemmas `lemma_toyoneda` and `lemma_fromyoneda` assert that the
two interpretation (`forall r. (a -> r) -> f r`) and (`f a`) are equal! So really, all Yoneda is doing
[from one point of view] is to tell us that we can create "enriched continuations".
