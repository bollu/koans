{-# LANGUAGE RankNTypes #-}
-- we need the forall m. to use the monoid structure twice

f :: (forall m. Monoid m => m -> m -> m) {- which might be mappend -} -> Int -> Int -> String -> String -> (Int,String)

