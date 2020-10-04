data Seq a = Nil | Cons a (Seq a)

last :: Seq a -> a
last (Cons x Nil) = x
last (Cons _ q) = last q

