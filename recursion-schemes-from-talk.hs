-- https://www.youtube.com/watch?v=Zw9KeP3OzpU

foldr :: (Maybe (a, b) -> b) -> [a] -> b
foldr alg [] = alg $ Nothing
foldr alg (x:xs) = alg $ Just(x, foldr alg xs)

