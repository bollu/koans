{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}

import Data.List
import Control.Applicative
import Data.Traversable
import qualified Data.Set as S
import qualified Data.Map as M
-- https://github.com/tjitze/ranked-programming/blob/master/documentation/ranked_programming.pdf

-- zero | successor | infinity
data Rank = RZero | RCons Rank | RInfty deriving(Eq)


radd RZero x = x
radd (RCons x) RZero = RCons x
radd (RCons x) (RCons y) = RCons (RCons (radd x y))
radd (RCons x) RInfty = RInfty
radd RInfty x = RInfty

rsub :: Rank -> Rank -> Rank
rsub RZero x = RZero
rsub (RCons x) RZero = RCons x
rsub (RCons x) (RCons y) = rsub x y
rsub (RCons x) (RInfty) = RZero
rsub RInfty RZero = RInfty
rsub RInfty (RCons _) = RInfty
rsub RInfty RInfty = RZero

instance Num Rank where
    (+) = radd
    (-) = rsub
    fromInteger 0 = RZero
    fromInteger x = RCons (fromInteger (x-1))


rank2num :: Rank -> Maybe Int
rank2num RZero = Just 0
rank2num (RCons x) = let (Just n) = rank2num x in Just (n+1)
rank2num RInfty = Nothing

instance Show Rank where
    show r = case rank2num r of Just n -> "r" ++ show n; Nothing -> "oo"

instance Ord Rank where
   compare RZero RZero = EQ
   compare RZero (RCons _) = LT
   compare RZero RInfty = LT
   -- cons
   compare (RCons _) RZero = GT
   compare (RCons x) (RCons y) = compare x y
   compare (RCons _) RInfty = LT
   -- infty
   compare RInfty _ = GT

radd :: Rank -> Rank -> Rank

data RankExpr a where
 Truth :: a ->  RankExpr a
 Falsity :: RankExpr a
 Choice :: RankExpr a -> Rank -> RankExpr a -> RankExpr a
 Observe :: (a -> Bool) -> RankExpr a -> RankExpr a -- what is this in the monad hierarchy?
 Call :: Ord b => RankExpr (a -> b) -> RankExpr a -> RankExpr b -- applicative
 -- Bind :: (a -> RankExpr b) -> RankExpr a -> RankExpr b -- monad?

-- | fn that when given a 'b', produces a rank

-- | given rank seen so far
mergeRanks :: Rank -> [(a, Rank)] -> [(a, Rank)] -> [(a, Rank)]
mergeRanks curr xs ys =
  case xs of
    [] -> ys
    (x,r):xs' -> case curr == r of
                   True -> (x,r):mergeRanks r xs' ys
                   False -> case ys of
                              [] -> []
                              -- can take ys till we reach r.
                              (y,s):ys' -> case curr == s of
                                             True -> (y,s):mergeRanks s xs' ys'
                                             False -> mergeRanks (min r s) xs' ys'
                  
removeDead :: [(a, Rank)] -> [(a, Rank)]
removeDead = filter (not . (== RInfty) . snd)

{-
instance Functor RankExpr where
    -- | (a -> b) -> RankExpr a -> RankExpr b
    fmap f x = Call (Truth f) x


instance Applicative RankExpr where
    pure = Truth
    (<*>) = Call

instance Alternative RankExpr where
    empty = Falsity
    (<|>) = choice

-}

choice :: RankExpr a -> RankExpr a -> RankExpr a
choice a b = Choice a 0 b



sem :: RankExpr a -> [(a, Rank)]
sem (Truth a) = [(a, RZero)] 
sem (Falsity) = []
sem (Choice u r e) = 
    removeDead $ mergeRanks 0 (sem u) [(v, r + rv) | (v, rv) <- sem e]
sem (Observe pred r) = 
    let rs = sem r
        ps = filter (pred . fst) rs
        leastrank = snd . head $ ps
    in [(vx, vrnk - leastrank) | (vx, vrnk) <- ps]


sem (Call rsf rsx) =
  produceCall 0 mempty (diagonalize (sem rsf) (sem rsx))

egTruth :: [(String, Rank)]
egTruth = sem $ Truth "a"

egChoice :: [(String, Rank)]
egChoice = sem $ Choice (Truth "a") 1 (Truth "b")

egRec :: RankExpr Int
egRec = Choice (Truth 1) 1 (Call (Truth (*2)) egRec)



-- . a  b  c  d ...
-- w aw bw cw dw
-- x ax bx cx dx
-- y ay by cy dy
-- z az bz cz dz
-- .

-- [] [] -> (a, w) => aw
-- [a] [w] -> (b, x) => bw | bx | ax
-- [a b] [w x] -> (c, y) => cw cx | cy | by ay 

dgnlz :: [a] -> [b] -> [a] -> [b]-> [(a, b)]
dgnlz [] [] _ _ = []
dgnlz (a:as) [] asfin bsfin = (map (a,) bsfin) ++ dgnlz as [] asfin bsfin
dgnlz [] (b:bs) asfin bsfin = (map (,b) asfin) ++ dgnlz [] bs asfin bsfin
dgnlz _ [] _ _  = []
dgnlz (a:as) (b:bs) asfin bsfin =
  let asfin' = a:asfin
      bsfin' = b:bsfin
  in (map (a,) bsfin) ++
     [(a,b)] ++
     (map (,b) asfin) ++
     dgnlz as bs asfin' bsfin'
  


-- productively produce from both a and b
diagonalize :: [a] -> [b] -> [(a, b)]
diagonalize as bs = dgnlz as bs [] []

-- see that we can only ever inflate ranks, so the rank of (f x) can be at most rank of f, rank of x?
produceCall :: Ord b
  => Rank -- rnk
  -> S.Set b -- seen
  -> [((a->b, Rank), (a, Rank))] -- fs 
  -> [(b, Rank)]
produceCall rnk seen (((f,r),(x,s)):rest) =
  let (y, t) = (f x, r + s) in (y,t):produceCall rnk seen rest
produceCall rnk seen [] = []
produceCall rnk seen (((f,r),(x,s)):rest) = 
  let (y, t) = (f x, r + s)
  in case S.member y seen of
      False ->
         let seen' = S.insert y seen
         in (y,t):produceCall rnk seen' rest
      True -> produceCall rnk seen rest
      
             
