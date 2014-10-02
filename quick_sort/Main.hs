qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = qsort [l | l <- xs, l <= p] ++ [p] ++
               qsort [r | r <- xs, r >  p]

main :: IO ()
main = print $ qsort [-2, 5, -11, 1, -3, -2, 3]
