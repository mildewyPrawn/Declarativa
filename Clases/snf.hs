module SFN where 

import Data.List
import Data.Maybe

data Nat = Zero | Suc Nat

instance Show Nat where
 show Zero = "0"
 show (Suc n) = show $ (read (show n)::Int) + 1  
 
minfree :: [Int] -> Int
minfree xs = head $ [0..] \\\ xs

(\\\) :: (Eq a) => [a] -> [a] -> [a]
xs \\\ ys = filter (\x -> not $ elem x ys) xs

loop :: [Int] -> Int -> Int
loop [] c = c
loop (x:xs) c
 | x == c = loop xs (c+1)
 | otherwise = c

minfree0 :: [Int] -> Int
minfree0 xs = loop (sort xs) 0

minfree1 :: [Int] -> Int
minfree1 xs = 
 fromMaybe (length xs) $ elemIndex False $ zipWith (==) [0..] (sort xs)

minfree2 :: [Int] -> Int
minfree2 xs = sfnAux $ (-1):(sort xs)

sfnAux :: [Int] -> Int
sfnAux (x:[]) = x + 1
sfnAux (x:(y:xs))
 | y - x > 1 = x + 1
 | otherwise = sfnAux (y:xs)
