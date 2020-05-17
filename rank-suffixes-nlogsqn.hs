import Data.List
import Control.Applicative
type Ix = Int
type SortedIx = Int
type Rank = Int
type Perm = [SortedIx]

-- Rank the tails of an array in O(rank) . O(log n) time. Here,
-- rank0 takes n^2 time.
--
-- We can use sort and compute rank in O(n log n)
-- time, giving total time of O(n log^2 n)
--
--
-- References: Richard bird, pearls of functional algorithm design

-- | parallel index
(!) :: [a] -> [Ix] -> [a]
(!) as = map (as !!)

-- | rank elements.
rank0 :: Ord a => [a] -> [Rank]
rank0 xs = map (\x -> length . filter (< x) $ xs) xs

-- | refine a ranking
(<<) :: Ord a => Ord b => [a] -> [b] -> [Rank]
(<<) xs ys = rank0 (zip xs ys)

rats :: Ord a => Int -> [a] -> [Rank]
rats k = rank0 . map (take k) . tails

-- rats (2∗k ) xs = rats k xs << shiftBy k (rats k xs)

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f:fs) a = if p a then a else applyUntil p fs (f a)

tails' :: [a] -> [[a]]
tails' [] = []; tails' x = x:tails' (tail x)

ranktails0 :: Ord a => [a] -> [Rank]; ranktails0 = rank . tails'

ranktails1 :: Ord a => [a] -> [Rank]
ranktails1 xs = applyUntil isperm rerankings . rank $  xs

rerankings :: [[Rank] -> [Rank]]
rerankings = map rerank (iterate (*2) 1)

rerank :: Rank -> [Rank] -> [Rank]
rerank k rs = rs << shiftBy k rs

shiftBy :: Int -> [Rank] -> [Rank]
shiftBy k rs = map (+k ) (drop k rs) ++ [k - 1, k - 2..0]

isperm :: [Int] -> Bool; isperm xs = nub xs == xs

applist :: [a -> a] -> a -> a
applist [] x = x
applist (f:fs) x = applist fs (f x)


ranktails2 xs = applyUntil isperm rerankings . rank

-- ranktails2 xs = applist  [\rs -> rank (zip rs ((map (+i) (drop i rs) ++  [i-1, i-2.. 0]))) | i <- takeWhile (< length xs) (iterate (*2) 1)] (rank xs)
shiftBy' :: Int -> [Rank] -> [Rank]; shiftBy' i xs = (drop i xs) <> replicate i (-1)
ranktails3 xs = applist [\rs -> rank0 (zip rs (shiftBy' i rs)) | i <- takeWhile (< length xs) (iterate (*2) 1)] (rank0 xs)

-- create an infinite stream of smallstrings with alphabet alpha
smallstrings :: [a] -- ^ alphabet
  -> Int -- ^ length
  -> [[a]]
smallstrings _ 0 = []
smallstrings alpha 1 = [[a] | a <- alpha]
smallstrings alpha n = let ss = smallstrings alpha (n-1) in ss <> (liftA2 (:) alpha ss)

mainrank :: IO ()
mainrank = do
  let ss = smallstrings [0, 1, 2] 9 :: [[Int]]
  let mismatches = [(s, ranktails0 s, ranktails3 s) |  s <- ss, ranktails0 s /= ranktails3 s ] 
  case mismatches of
    [] -> print "Success"
    (x:xs) -> print $ "Failure: " <> show x


rank2sort :: [Rank] -> [SortedIx]
rank2sort rs = map snd $ sort (zip rs [0..])


sorttails0 :: Ord a => [a] -> [[a]]
sorttails0 = sort . tails'

mainsort :: IO ()
mainsort = do
  let ss = smallstrings [0, 1, 2] 9 :: [[Int]]
  let mismatches = [(s, sorttails0 s, tails' s  ! (rank2sort (ranktails3 s))) |  s <- ss, sorttails0 s /= (tails' s) ! rank2sort (ranktails3 s) ] 
  case mismatches of
    [] -> print "Success"
    (x:xs) -> print $ "Failure: " <> show x
