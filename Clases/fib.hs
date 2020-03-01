module Fib where

 fibD :: Int -> Int 
 fibD 0 = 0
 fibD 1 = 1
 fibD n = fibD (n - 1) + fibD (n - 2)

 fibT :: Int -> (Int, Int) 
 fibT 0 = (0,1)
 fibT n = (y, x+y)
  where (x,y) = fibT (n-1)

 fibB :: Double -> Double 
 fibB n = (((1+ (sqrt 5))/2)**n - ((1-(sqrt 5))/2)**n) /(sqrt 5)

 fibV :: Int -> (Int, Int)
 fibV 0 = (0, 1)
 fibV n
  | even n = (c,d)
  | otherwise = (d, c + d)
  where (a, b) = fibV (div n 2)
        c = 2 * a * b - a * a
        d = a * a + b * b

 fibM :: Int -> Int
 fibM = (map f [0 ..] !!) 
  where f 0 = 0
        f 1 = 1
        f n = fibM (n-2) + fibM (n-1)
