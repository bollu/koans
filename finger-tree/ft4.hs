data Seq a = Nil | Unit a | More (Some a) (Seq (Tuple a)) (Some a)
data Some a = One a | Two a a
data Tuple a = Pair a a | Triple a a a

-- head/cons is trivial.
tail :: Seq a -> Seq a
tail (Unit _) = Nil
tail (More (One _) q u) = more0 q u 
tail (More (Two _ x) q u) = More (One x) q u


more0 :: Seq (a, a) -> Some a -> Seq a
more0 Nil (One y) = Unit y
more0 Nil (Two y z) = More (One y) Nil (One z)
more0 q u = case head q of 
              Pair x y -> More (Two x y) (tail q) u
              Triple x y z -> -- ? More (x y) (cons z (tail q)) u

Consider

tail (More (One 'a') (cons (Triple 'b' 'c' 'd') q) u). Has two answers:
- tail (More (One 'b') (cons (Pair 'c' 'd') q) u) <- we chooe this, is the only natural thing.
- tail (More (Two 'b' 'c') (cons (<only 'd'>) q) u)


-- | apply fn to first  Some of a queue
replaceHead :: Some a -> Seq a -> Seq a
replaceHead sa (More _ q u) = More sa q u

more0 :: Seq (a, a) -> Some a -> Seq a
more0 Nil (One y) = Unit y
more0 Nil (Two y z) = More (One y) Nil (One z)
more0 q u = case head q of 
              Pair x y -> More (Two x y) (tail q) u
              Triple x y z -> More (One x) (replaceHead (Pair y q) q) u 

-- (++)
toList :: Some a -> [a]; 


-- tail takes sequence starts with a One to a sequence that starts with Two.
-- tail (One _) -> Two -- expensive!
-- tail (Two _) -> One -- cheap!
--
-- cons (One _) -> Two -- cheap!
-- cons (Two _) -> One -- expensive!
--
-- We can add a case that is cheap for *both* of these operators. Then we have
-- a chance at getting constant time amortized complexity.
--
--
-- we add one more case by allowing three elements.
--
-- tail (One _) -> Two -- expensive!
-- tail (Two _) -> One -- cheap!
-- tail (Three _) -> Two -- cheap!
--
--
-- cons (One _) -> Two -- cheap!
-- cons (Two _) -> Three -- cheap!
-- cons (Three _) -> Two -- expensive!
--
--
-- cons takes sequence starts with Two One to a sequence that starts with One.
-- so these enables each others reucrsion to be log time.
