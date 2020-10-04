-- | Implicit recursive slowdown by Chris Okasaki
data Seq a = Nil | Unit a | More a (Seq (a, a)) a

thunk :: a; thunk = thunk

cons :: a -> Seq a -> Seq a
cons x Nil = Unit x
cons x (Unit y) = More y Nil y
cons x (More y q z) = More x (cons (y, thunk) q) z -- impossible!

We can't represent sequences with 3 elements. If we allow some slack,
then we can allow this.



