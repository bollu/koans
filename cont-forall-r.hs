{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}


 data Cont a = (forall r. Cont ((a -> r) -> r))

purex :: a -> Cont a
purex a = Cont (\f -> f a)

