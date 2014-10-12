data Vector = V Double Double
data Matrix = M Double Double
                Double Double

innerProd :: Vector -> Vector -> Double
innerProd (V a1 a2) (V b1 b2) = a1 * b1 + a2 * b2

prodVM :: Vector -> Matrix -> Vector
prodVM (V a1 a2) (M b11 b12
                    b21 b22) = (V (innerProd (V a1 a2) (V b11 b21))
                                  (innerProd (V a1 a2) (V b12 b22)))

showV :: Vector -> String
showV (V a1 a2) = "("++ (show a1) ++", "++ (show a2) ++")"

main :: IO ()
main = do
    print $ innerProd (V 4.0 2.0) (V 1.0 2.0)
    print $ showV $ prodVM (V 4.0 2.0) (M 2.0 3.0
                                          7.0 2.0)
