-- | rearrange to get new repr: finger
data Seq a = Nil | Unit a | More a (Seq a) a 


-- | O(1)
head :: Seq a -> a; head (Unit x) = x; head (More x _ _) = x;
-- | O(1)
last :: Seq a -> a; head (Unit x) = x; head (More _ _ x) = x;
cons :: a -> Seq a -> Seq a
-- | O(1)
cons Nil = Unit a; cons x (Unit y) = More x Nil y
-- | O(n); Next idea: make recursion faster
cons x (More y q z) = More x (cons y q) z

-- If we have some datatype `Seq a`, and we build `Seq2 (a, a)`, `Seq2` is
-- "twice as fast" because it lets us seek any position in half the time.
-- If we do this recursively, we get log time.
