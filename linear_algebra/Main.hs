data Vector = Vec Double Double

innerProd :: Vector -> Vector -> Double
innerProd (Vec ax ay) (Vec bx by) = ax * bx + ay * by

main :: IO ()
main = print $ innerProd (Vec 4.0 2.0) (Vec 1.0 2.0)
