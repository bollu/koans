-- untrue quicksort

qsort [] = []
qsort (x:xs) = qsort [l | l <- xs, l < x] ++ [x] ++ qsort [g | g <- xs, g > x]
