{-# LANGUAGE TupleSections #-}
import Data.Monoid
import qualified Data.Vector.Mutable as V 
import qualified Data.Vector as Vimmut
import Data.Foldable
import Control.Monad.ST
import Control.Exception
import Debug.Trace

-- *** naive permutations ***
-- split array at index
split :: Int -> [a] -> ([a], [a])
split i xs = (take i xs, drop i xs)

-- insert the value into every position in the array from start to end
insertAll :: a -> [a]-> [[a]]
insertAll a arrs = fmap (\(l, r) -> l ++ [a] ++ r) splits where 
  splits = [split i arrs | i <- [0..(length arrs)]]

-- naive implementation of permutations
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = mconcat $ fmap (insertAll x) (perms xs)



-- *** johnson-trotter ***
-- [https://en.wikipedia.org/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm]
data Direction = L | R deriving(Show, Eq)
data Directed a = Directed Direction a deriving (Eq)

instance Show a => Show (Directed a) where
  show (Directed L a) = "<" <> show a
  show (Directed R a) = ">" <> show a

swapDir :: Directed a -> Directed a
swapDir (Directed dir a) = Directed (if dir == L then R else L) a

mkDirected :: Functor f => f x -> f (Directed x)
mkDirected xs = fmap (Directed L)  xs

greatestmobile :: (Show a, Ord a) => V.STVector s (Directed a) -> ST s (Int, Maybe (Directed a))
greatestmobile xs = foldrM 
  (\i (gi, mg)-> do
                  dirx @ (Directed _ x) <- V.read xs i
                  mobile <- ismobile i xs
                  --  trace ("i: " <> (show i) <>
                  --        " | dirx: " <> (show dirx) <>
                  --        " | mobile? " <>
                  --        (show mobile)) (return ())
                  return $ case mg of
                            Nothing -> if mobile then (i, Just dirx) else (gi, mg)
                            Just (dirg @ (Directed _ g)) -> 
                              if mobile && x > g  
                              then (i, Just dirx)
                              else (gi, mg)) 
  (-1, Nothing) -- HACK
  [0..(V.length xs - 1)]
  

ismobile :: (Show a, Ord a) => Int -> V.STVector s (Directed a) -> ST s Bool
ismobile i xs = do
  dirx@(Directed dir x) <- V.read xs i
  if dir == R && i >= (V.length xs - 1)
  then return False
  else
    if dir == L && i <= 0
    then return False
    else  do
             let adji = if dir == R then i + 1 else i - 1
             diradj @ (Directed _ adj) <- V.read xs adji
             -- trace ("(i: " <> show i <> ", x: " <> (show dirx) <> ")" <> 
             --        " >? " <> 
             --        "(adji: " <> show adji <> ", adjx: " <> (show diradj) <> ")" <> 
             --        " = " <>
             --       show (x >= adj)) (return ())
             return $ x >= adj -- >= or >?
    

move :: Int -> V.STVector s (Directed a) -> ST s ()
move i xs = do
  (Directed dir x) <- V.read xs i
  let newi = case dir of
              L -> i - 1
              R -> i + 1
  V.swap xs i newi


reversegt :: Ord a => Directed a -> V.STVector s (Directed a) -> ST s ()
reversegt (Directed _ gt) xs = 
  forM_ [0..(V.length xs - 1)]
    (\i -> do
      dir @ (Directed _ x) <- V.read xs i
      if x > gt
      then V.write xs i (swapDir dir)
      else return ())

jt' :: (Ord a, Show a) => V.STVector s (Directed a) -> ST s ()
jt' xs = do
  (i, mx) <- greatestmobile  xs
  -- trace ("gm: " ++ (show i ) <> " : " <> (show mx)) (return ())
  case mx of
    Nothing -> return ()
    Just x -> move i xs >> reversegt x xs

jt :: (Ord x, Show x) => [Directed x] -> [Directed x]
jt xs = runST $ do
  xsvec <- Vimmut.thaw (Vimmut.fromList xs)
  jt' xsvec
  Vimmut.toList <$> Vimmut.freeze xsvec

-- *** heaps' algorithm ***
