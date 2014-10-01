max2Impl :: [Int] -> Int -> Int
max2Impl [] m = m
max2Impl (x:xs) m | m < x     = max2Impl xs x
                  | otherwise = max2Impl xs m

max2 :: [Int] -> Int
max2 list = max2Impl list (-9999)

main :: IO ()
main = print $ max2 [3,2,1,5,-4]
