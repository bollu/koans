{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'


type End (p :: * -> * -> *) = forall x. p x x
data Coend p where
  MkCoend :: p a a -> Coend p

-- Hom(\int^x p(x, x), r) ~= \int_x (Hom(p x x, r)) 

type Hom a b = a -> b
type RHS p r a b = Hom (p a b) r



-- fwd :: (Coend p -> r) -> End (RHS p r)
-- fwd :: (Coend p -> r) -> (forall x. (RHS r) x x)
-- fwd :: (Coend p -> r) -> (forall x. Hom (p x x) r)
-- fwd :: (Coend p -> r) -> (forall x. (p x x) -> r)
fwd :: Profunctor p => (Coend p -> r) -> (forall x. (p x x) -> r)
fwd coendp2r  pxx = coendp2r (MkCoend pxx)


-- bwd :: End (RHS p r)             -> (Coend p -> r) 
-- bwd :: (forall x. (RHS r) x x)   -> (Coend p -> r) 
-- bwd :: (forall x. Hom (p x x) r) -> (Coend p -> r) 
-- bwd :: (forall x. (p x x) -> r)  -> (Coend p -> r) 
bwd :: Profunctor p => (forall x. (p x x) -> r) -> Coend p -> r
bwd pxx2r (MkCoend paa) = pxx2r paa

-- ninja coyoneda lemma:
-- \int^x C(x, a) * f(x) ~= f(a)
-- the profunctor is \int^x NinjaLHS[f, a](x, y)
--   where
newtype NinjaLHS g b y z = MkNinjaLHS (y -> b, g z)


-- ninjaFwd :: Functor f => Coend (NinjaLHS f a) -> f a
ninjaFwd :: Functor g => Coend (NinjaLHS g r) -> g r
ninjaFwd (MkCoend (MkNinjaLHS (x2r, gx))) = fmap x2r gx

-- ninjaBwd :: Functor f => g r -> (Coend (NinjaLHS g r))
-- ninjaBwd :: Functor f => g r -> (∃ x. (NinjaLHS g r x x))
-- ninjaBwd :: Functor f => g r -> (∃ x. (NinjaLHS (x -> r, g x))
ninjaBwd :: Functor g => g r -> Coend (NinjaLHS g r)
ninjaBwd gr = MkCoend (MkNinjaLHS (x2r, gx)) where
   x2r = id -- choose x = r, then x2r = r2r 
   gx = gr -- choose x = r


-- newtype NinjaLHS' g i o = MkNinjaLHS' (i -> o, g i)
newtype NinjaLHS' g i o = MkNinjaLHS' (NinjaLHS g o i i)

mkNinjaLHS' :: g i -> NinjaLHS' g i i
mkNinjaLHS' gi = MkNinjaLHS' (MkNinjaLHS (id, gi))


instance Functor (NinjaLHS' g i) where
  -- f:: (o -> o') -> NinjaLHS' g i o -> NinjaLHS' g i o'
  fmap o2o' (MkNinjaLHS' (MkNinjaLHS (i2o, gi))) = 
    MkNinjaLHS' $ MkNinjaLHS (\i -> o2o' (i2o i), gi)


extract :: Functor g => NinjaLHS' g i o -> g o
extract (MkNinjaLHS' (MkNinjaLHS (i2o, gi))) = fmap i2o gi
