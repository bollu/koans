The `bound` library
============================

At the end of this blog post, we'll know how to use `ekmett/bound`,
a library that provides a convenient monad for manipulating bound
variables when implementing interpreters.


> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE TemplateHaskell #-}
> import Bound
> import Control.Applicative
> import Control.Monad (ap)
> import Data.Functor.Classes
> import Data.Foldable
> import Data.Traversable
> 
> module Main where
> main :: IO (); main = print "hello"
> type Name = String
> 
> data Exp 
>   = Var Name  -- x
>   | App Exp Exp -- f e
>   | Lam Name Exp -- \x. e
>   deriving (Eq, Show)
> 
> main = do
>   print $ Var "x"
>   print $ Lam "x" (Var "x")
>   print $ Lam "x" (Lam "y" (Var "x"))

#### Capture avoidance

Substituting `znew = Lam "x" (Var "y")` into `e' = Lam "y" (Var "z")`
for `z` would yield `e[z/znew] = Lam "y" (Lam "x" (Var "y"))`.
This is not what we want, because the "y" that was referring to something
outside the scope in "z" now refers to the "y" in `e'`. 

#### Equality testing (Alpha equivalence)

`λx.x` and `λy.y` both mean the same thing, so we want them to evaluate to be equal.

#### Common solutions

The two solutions that are most often used is to [substitute with renaming](TODO),
or to use [de bruijn indeces](TODO), both of which have their own trade-offs. In this
blog post, we'll check out how to use the `bound` library for this task.


>infixl 9 :@
> data Exp'  a = Var' a | App' (Exp' a) (Exp' a)| Lam' (Scope () Exp' a)
>  deriving (Functor,Foldable,Traversable) 

> instance Applicative Exp where pure = Var; (<*>) = ap
> instance Monad Exp where
>   return = Var
>   Var' a      >>= f = f a
>   (App' eg ex) >>= f = App' (eg >>= f) (ex >>= f)
>   Lam e    >>= f = Lam (e >>>= f) -- >>>=
> deriveEq1   ''Exp
> deriveOrd1  ''Exp
> deriveRead1 ''Exp
> deriveShow1 ''Exp
> 
> instance Eq a   => Eq   (Exp a) where (==) = eq1
> instance Ord a  => Ord  (Exp a) where compare = compare1
> instance Show a => Show (Exp a) where showsPrec = showsPrec1
> instance Read a => Read (Exp a) where readsPrec = readsPrec1
> 
> lam :: Eq a => a -> Exp a -> Exp a
> lam v b = Lam (abstract1 v b)
> 
> whnf :: Exp a -> Exp a
> whnf (f :@ a) = case whnf f of
>   Lam b -> whnf (instantiate1 a b)
>   f'    -> f' :@ a
> whnf e = e


