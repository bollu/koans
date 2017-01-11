-- Implementation of Bananas, Lenses and Barbed Wire
data Star a = Nil |  Cons a  (Star a) deriving (Show, Eq)

cata :: b -> (a -> b -> b) -> Star a -> b
cata b combine = 
  let h = \x -> case x of
                  Nil -> b
                  Cons a as -> combine a (h as)
  in h


ana :: (b -> Bool) -> (b -> (a, b)) -> b -> Star a
ana p g =
  let h = \b -> if p b
                then Nil
                else 
                  let (a, b') = g b in Cons a (h b')
  in h


-- zip as ana
zip xs ys = ana (\(xs, ys) -> (null xs) || (null ys)) (\((x:xs), (y:ys)) -> ((x, y), (xs, ys))) (xs, ys)


-- iterate as ana
iterate :: (a -> a) -> a -> Star a
iterate f x = ana (const False) (\x -> (x, f x)) x


-- TODO: map as both cata and ana


hylo :: c -> (b -> c -> c) -> (a -> (b, a)) -> (a -> Bool) -> a -> c
hylo c combine g p = 
      let
          h = \a -> if p a
                    then c
                    else
                      let (b, a') = g a
                      in combine b (h a')
       in h


fact n = hylo 1 (*) (\x -> (x, x - 1)) (== 0) n



class Bifunctor f where
  bimap :: (a -> b, c -> d) -> (f a c -> f b d)
