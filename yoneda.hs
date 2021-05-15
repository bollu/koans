{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

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
