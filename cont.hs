{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

-- Encoding the coninuation monad in Haskell

data Cont r a = Cont {
  cont :: (a -> r) -> r
}

(#>) :: (a -> r) -> Cont r a -> r
(#>) f c = cont c $ f

(<#) :: Cont r a -> (a -> r) -> r
(<#) = flip (#>)

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f c = Cont (\buser -> (buser . f)  #> c)
 
instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont (\auser -> auser a)

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) catob ca = Cont (\btor -> 
      ca <# (\a -> (
                catob <# (
                  \atob -> btor . atob $ a))))


join :: Cont r (Cont r a) -> Cont r a
join cca = Cont (\auser -> cca <# (\ca -> ca <# auser))

instance Monad (Cont r) where
  return :: a -> Cont r a
  return = pure

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) ca atocb = Cont (\buser -> ca <# (\a -> (atocb a) <# buser))


constCont :: r -> Cont r x
constCont r = Cont $ \_ -> r

callCC :: ((forall x. a -> Cont r x) -> Cont r a) -> Cont r a
callCC atocx_toca = Cont (\auser -> atocx_toca (\a -> constCont (auser a)) <# auser)

main :: IO ()
main = undefined
