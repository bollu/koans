 data Seq a = Nil | Unit a | More (Some a) (Seq (a, a)) | (Some a)
 data Some a = One a | Two a a

 head :: Seq a -> a; head (Unit x) = x;
 head (More (One a) _ _) = a; head (More (Two a _) _ _) = a

 last/init/snoc are symmetric.

 cons :: a -> Seq a -> Seq a
 cons x Nil = Unit x
 cons x (Unit y) = More (One x) Nil (One y)
 cons x (More (One y) q u) = More (Two x y) q u)
 -- | number of recursions is log(n)
 cons x (More (Two y z) q u) = (More  (Two x y) (cons (y,z) q) u)

 -- | tail is recursive. logarithmic.
 tail :: Seq a -> Seq a
 tail (Unit _) = Nil
 tail (More (One _) q u) = more0 q u 
 tail (More (Two _ x) q u) = More (One x) q u


 more0 :: Seq (a, a) -> Some a -> Seq a
 more0 Nil (One y) = Unit y
 more0 Nil (Two y z) = More (One y) Nil (One z)
 more0 q u = case head q of (x, y) -> More (Two x y) (tail q) u

-- What about (++)?
-- consider More u p (Two 'a' 'b') ++ More (One 'c') q v
-- ~= More u (p ++ ['a', 'b', 'c'] ++ q) v
--
-- how is this possible? `p`, `q` are sequences of pairs. We can't insert
--    3 elements into them! We can implemnt it, but the complexity will not be nice.
-- EXERCISE: implement this!
-- So we choose to allow ourselves to have either pairs or triples in the inner Seq.
