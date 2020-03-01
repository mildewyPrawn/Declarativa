{-
- Programacion Declarativa 2020-1
- Tarea 3:  Bringing you into the fold
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Tarea3 where

--------------------------------------------------------------------------------
--------                        PRIMERA PARTE                           --------
--------------------------------------------------------------------------------

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat xss = foldr (++) [] xss

myMinimum :: (Ord a) => [a] -> a
myMinimum (x:xs) = foldr min x xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldr (\b g x -> g (b : x)) id xs []

myFilter :: (a->Bool) -> [a] -> [a]
myFilter p (x:xs) = foldr (\x xs -> if (p x) then x:xs else xs) [] xs

myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits (x:xs) = foldr (\x-> init x) [] xs


--x '(\x xs -> xs)'
{-
> foldr f z []     = z
> foldr f z (x:xs) = x `f` foldr f z xs
-}



