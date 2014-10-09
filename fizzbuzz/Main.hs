fizzbuzz :: Int -> [String]
fizzbuzz n = impl n 1
  where
    impl m i
        | i > m         = []
        | mod i 15 == 0 = "FizzBuzz" : impl m (i+1)
        | mod i  5 == 0 = "Buzz"     : impl m (i+1)
        | mod i  3 == 0 = "Fizz"     : impl m (i+1)
        | otherwise     = show i     : impl m (i+1)

fizzbuzz2 :: Int -> [String]
fizzbuzz2 n =
    zipWith select (map show [1..n]) $
    zipWith select (make 3 "Fizz") $
    zipWith select (make 5 "Buzz") (make 15 "FizzBuzz")
  where
    make m s = concat . concat . repeat $ replicate (m-1) [""] ++ [[s]]
    select a b = if null b then a else b

main :: IO ()
main = do
    print $ fizzbuzz 100
    print $ fizzbuzz2 100
