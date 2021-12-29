{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Monad.Fix

data T fix f bix b a = T { runt :: (fix -> f, bix -> b) -> (a, fix -> f, bix -> b) } deriving(Functor)
instance Applicative (T fix f bix b) where pure = return; (<*>) = ap
instance Monad (T fix f bix b) where 
  return x = T $ \ ~(f, b) -> (x, f, b)
  tx >>= x2ty = T $ \ ~(f, b) -> 
   let ~(x, f', b'') = runt tx (f, b')
       ty = x2ty x
       ~(y, f'', b') = runt ty (f', b)
    in (y, f'', b'')

instance MonadFix (T fix f bix b) where 
   mfix f = do
      rec x <- (f x)
      return x

setfwdval :: Eq fix => fix -> f -> T fix f bix b ()
setfwdval vi fv = T $ \ ~(f, b) -> ((), \i -> if i == vi then fv else f i, b)

setbwdval :: Eq bix => bix -> b -> T fix f bix b ()
setbwdval vi bv = T $ \ ~(f, b) -> ((), f, \i -> if i == vi then bv else b i)

getbwdval :: bix -> T fix f bix b b
getbwdval bi = T $ \ ~(f, b) -> (b bi, f, b)

getfwdval :: fix -> T fix f bix b f
getfwdval fi = T $ \ ~(f, b) -> (f fi, f, b)


type Name = String
type I x = T Name Int Name Int x


-- V2.
-- | values flow forward, gradients flow backward
getvar :: Name -> I Int
getvar n = getfwdval n

-- | set value
setvar :: Name -> Int -> I ()
setvar n i = setfwdval n  i

getgrad :: Name -> I Int
getgrad n = getbwdval n

setgrad :: Name -> Int -> I ()
setgrad n i = setbwdval n i

accumgrad :: Name -> Int -> I ()
accumgrad n i = T $ \ ~ (f, b) -> ((), f, \ncur -> (if ncur == n then i else 0) + b ncur)

-- x = 10
-- xsq = x * x
p1 :: I ()
p1 = do
 setvar "x" 2
 setvar "y" 3

 -- xsq
 l <- getvar "x"; r <- getvar "y"
 setvar "xy" (l * r)

 d_xsq <- getgrad "xy"
 accumgrad "x" (d_xsq * r); accumgrad "y" (d_xsq * l)

 setgrad "xy" 1

(v, vals, grads) =  runt p1 (const 42, const 0)
runthis = (vals "x", vals "y", vals "xy", grads "x", grads "y", grads "xy")

-- | multiply two values
mul :: Name -> Name -> Name -> I ()
mul x nl nr = do
 l <- getvar nl; r <- getvar nr
 setvar x (l * r)
 ds_dx <- getgrad x
 accumgrad nl (ds_dx * r)
 accumgrad nr (ds_dx * l)

add :: Name -> Name -> Name -> I ()
add x nl nr = do
 l <- getvar nl; r <- getvar nr
 setvar x (l + r)
 ds_dx <- getgrad x
 accumgrad nl (ds_dx)
 accumgrad nr (ds_dx)

p2 :: I ()
p2 = do
 setvar "x" 2
 setvar "y" 3

 mul "xsq" "x" "x"
 mul "xy" "x" "y"

 add "z" "xsq" "xy"

 setgrad "z" 1

(_, vals2, grads2) =  runt p2 (const 0, const 0)
runthis2 = (vals "x", vals "y", vals "xy", grads "x", grads "y", grads "xy")
