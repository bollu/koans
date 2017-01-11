{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- Haskell encoding of DSLs using Free and Cofree


import Data.Map
import Debug.Trace

type Key = String
type Value = String

data DSL next = Get Key (Maybe Value -> next) |
                Set Key Value next |
                End deriving (Functor)

data Interp next = Interp {
  getH :: Key -> (Maybe Value, next),
  setH :: Key -> Value -> next
} deriving(Functor)



data Free f a = Roll (f (Free f a)) | Pure a 

instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure a) = Pure $ f a
  fmap f (Roll f_freea) = Roll $ (fmap . fmap) f f_freea

instance Functor f => Monad (Free f) where
  return :: a -> Free f a
  return a = Pure a

  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (>>=) (Pure a) f = f a
  (>>=) (Roll f_freea) f = Roll $ fmap (\free -> free >>= f) f_freea

instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure = return

  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (<*>) free_ab free_a = do
          ab <- free_ab
          a <- free_a
          return $ ab a

fend :: Free DSL ()
fend = Roll End

fget :: Key -> Free DSL ()
fget k  = Roll (Get k (\v -> Pure ()))

fset :: Key -> Value -> Free DSL ()
fset k v = Roll (Set k v (Pure ()))

runDSL :: Free DSL () -> IO ()
runDSL dsl = runDSL' (Data.Map.fromList []) dsl

runDSL' :: Map Key Value -> Free DSL () -> IO ()
runDSL' m (Roll End) = putStrLn "Done"

runDSL' m (Roll (Set k v n)) = do
    putStrLn  $ "SET: <" ++ (show k) ++ "," ++ (show v) ++ ">"
    let m' = insert k v m
    runDSL' m' n

runDSL' m (Roll (Get k fn))  = do
    let mv = Data.Map.lookup k m
    case mv of
        Nothing -> putStrLn $ "GET: " ++ (show k) ++ " => " ++ "_"
        Just v -> putStrLn $  "GET: " ++ (show k) ++ " => "++ (show v) 
    runDSL' m (fn mv)

runDSL' m (Pure x) = do
    putStrLn ("WRONG ENDING TO PROGRAM")

y = fget "10" >> fget "0"
x =  fget "10" >> fset "10" "20" >> fget "10" >> fend



class Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  (=>>) :: w a -> (w a -> b) -> w b

data Cofree f a = a :< (f (Cofree f a)) 

deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)


instance Functor f => Functor (Cofree f) where
  fmap :: (a -> b) -> Cofree f a -> Cofree f b
  fmap g (a :< f_as) = g a :< ((fmap . fmap) g f_as)

instance Functor f => Comonad (Cofree f) where
  extract :: Cofree f a -> a
  extract (a :< _) = a
    
  duplicate :: Cofree f a -> Cofree f (Cofree f a)
  duplicate (c @ (_ :< f_as)) = c :< (fmap duplicate f_as)

  (=>>) :: Cofree f a -> (Cofree f a -> b) -> Cofree f b
  (=>>) wa f = fmap f $ duplicate wa 

coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter g seed = seed :<  (coiter g <$> g seed)

-- an insatnce of the cofree interpreter
coInterp :: Cofree Interp (Map Key Value)
coInterp = coiter next start where
    next map = Interp (coGet map) (coSet map)
    start = Data.Map.fromList []

coGet :: Map Key Value -> Key -> (Maybe Value, Map Key Value)
coGet m k =  (Data.Map.lookup k m, m)

coSet :: Map Key Value -> Key -> Value -> Map Key Value
coSet m k v = insert k v m


class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair :: (a -> b -> r) -> Cofree f a -> Free g b -> r
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Roll gs) = pair (pair p) fs gs

-- Pairing is essential to this code
-- what would happen if the pairing coule be hidden?
-- this needs all of the DSL
instance Pairing Interp DSL where
  pair :: (a -> b -> r) -> Interp a -> DSL b -> r
  pair f (Interp getter _) (Get key next) = f a' b' where
    (valMaybe, a') = getter key
    b' = next valMaybe


  -- setter :: Key -> Value -> a
  -- Set (key :: Key) (value  :: Value) (next :: b)
  pair f (Interp _ setter) (Set key value next) = f a' next where
    a' = setter key value

  pair f _ End = trace "reached end, don't know what to do" undefined

runInterpDSL :: (Cofree Interp) a -> (Free DSL) b -> a
runInterpDSL interp dsl = pair (\a b -> a) interp dsl

