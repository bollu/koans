-- https://gist.github.com/PkmX/182c6c5444b695d9a794


loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs
  where xs = fmap ($ xs) fs

