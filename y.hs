y :: (a -> a) -> a
y f = f (y f)

fact = y (\f a -> if a == 0 then 1 else a * f(a - 1))
