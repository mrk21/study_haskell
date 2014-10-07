fizzbuzzImpl :: Int -> Int -> [String]
fizzbuzzImpl n i | i > n           = []
                 | (mod i 15) == 0 = "FizzBuzz" : (fizzbuzzImpl n (i+1))
                 | (mod i  5) == 0 = "Buzz"     : (fizzbuzzImpl n (i+1))
                 | (mod i  3) == 0 = "Fizz"     : (fizzbuzzImpl n (i+1))
                 | otherwise       = (show i)   : (fizzbuzzImpl n (i+1))

fizzbuzz :: Int -> [String]
fizzbuzz n = fizzbuzzImpl n 1

main :: IO ()
main = print $ fizzbuzz 100
