{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Functor.Identity
import Data.Functor.Constant
 
{- o := Object, a := Attribute
 - Lens o a :: (o -> a, o -> a -> o)
 -          = o -> (a, a -> o)
 -          = o -> Store a o
 - Lens o a = o -> Store a o
 -}

data Store s a = Store { value :: s, get :: s -> a}


instance Functor (Store s) where
  --fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store{..}) = Store {
    value = value,
    get = f . get 
  }

type Pretext b a = forall f. Functor f => (b -> f b) -> f a


iso_forward :: Store s a -> Pretext s a
iso_forward (Store{..}) = \f  -- :: forall f. Functor f => (s -> f s)
                            -> fmap  get  (f value)
-- Functor = Store b
-- type Pretext b a = forall f. Functor f => (b -> f b) -> f a
-- type Pretext s a =  (s -> Store s s) -> Store s  a
iso_backward :: Pretext s a -> Store s a
iso_backward pretext = pretext (\s -> Store { value = s, get = id}) 


-- Lens o a = o -> Store a o
-- Lens o a = o -> Pretext a o
-- Lens o a = o -> (forall f. Functor f => (a -> f a) -> f o)
-- Lens o a = (forall f. Functor f => (a -> f a) -> (o -> f o)

type Lens o a = forall f. Functor f => (a -> f a) -> (o -> f o)

_1 :: Lens (a, b) a
_1 f ((a, b)) = fmap (\a -> (a, b)) (f a)

_2 :: Lens (a, b) b
_2 f ((a, b)) = fmap (\b -> (a, b)) (f b)


edit :: Lens o a -> (a -> a) -> o -> o
edit l f o = runIdentity (l (id_f) o)
  where
    -- id_f :: (a -> Identity f)
    id_f = Identity . f

set :: Lens o a -> a -> o -> o
set l a o = edit l (const a) o

type Getter o a = (a -> Constant a a) -> (o -> Constant a o)

getter :: Getter o a -> o -> a
getter l o = getConstant (l (\a -> Constant a) o)

(^.) :: o -> Getter o a -> a
(^.) o l = getter l o

to :: (o -> a) -> (a -> Constant a a) -> (o -> Constant a o)
to f _ o = Constant (f o)
