{-
- Programacion Declarativa 2020-1
- Tarea 3:  Bringing you into the fold
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Tarea3 where

import Data.Function
import Data.Maybe

---------------------------------------------------------------------------------
--------                        PRIMERA PARTE                           ---------
---------------------------------------------------------------------------------

-- | myConcat. Función 'concat' como instancia de foldr.
myConcat :: [[a]] -> [a]
myConcat = foldr (++) [] 

-- | myMinimum. Función 'minimum' como instancia de foldr.
myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "No hay elementos"
myMinimum (x:xs) = foldr min x xs

-- | myReverse. Función 'reverse' como instancia de foldr.
myReverse :: [a] -> [a]
myReverse xs = foldr (\b g x -> g (b : x)) id xs []

-- | myFilter. Función 'filter' como instancia de foldr.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p xs = foldr (\x xs -> if (p x) then x:xs else xs) [] xs

-- | myInits. Función 'inits' como instancia de foldr
myInits :: [a] -> [[a]]
myInits = foldr (\x y -> [] : (map (x:) y )) [[]]

---------------------------------------------------------------------------------
--------                        SEGUNDA PARTE                           ---------
---------------------------------------------------------------------------------

-- | foldi. Operador de plegado para el tipo Int.
foldi :: (a -> a) -> a -> Int -> a
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

---------------------------------------------------------------------------------
--------                        TERCERA PARTE                           ---------
---------------------------------------------------------------------------------

-- | sumq. Función que recibe un entero n y regresa la suma de los cuadrados de
-- los primeros n números
sumq :: Int -> Int
sumq n = foldl (\x y -> x+y) 0 (map (^2) [0..n])

-- | remove. Función que toma dos listas y elimina todos los elementos de la
-- segunda que aparecen en la primera.
remove :: (Eq a) => [a] -> [a] -> [a]
remove xs = myFilter (`notElem` xs)

-- | remdups. Elimina los elementos adyacenes duplicados en una lista.
remdups :: (Eq a) => [a] -> [a]
remdups xs = foldl (\l x -> if l == []
                            then [x]
                            else if last l == x then l else l++[x]) [] xs

-- | rotate. Produce todas las posibles rotaciones de una lista.
rotate :: [a] -> [[a]]
rotate [] = []
rotate xs = scanl (\x y -> shift x++y ) xs
            (fixpoint (\ff n -> if n == 0
                                then []
                                else [[]] ++ ff(n-1)) $ (length xs) - 1)

---------------------------------------------------------------------------------
--------                         CUARTA PARTE                           ---------
---------------------------------------------------------------------------------

unmerge :: (Ord a) => [a] -> [([a],[a])]
unmerge = error "No sé que hace unmerge \129312"
-- unmerge xs = [ (ys,zs) | merge ys zs == xs]


---------------------------------------------------------------------------------
--------                          AUXILIARES                             --------
---------------------------------------------------------------------------------

-- | shift. Función auxiliar que calcula una rotación de una lista.
shift :: [a] -> [a]
shift (x:xs) = foldl (\x y -> y:x) [x] (myReverse xs) -- DUDA

-- | fixpoint. FIXPOINT
fixpoint f x = f (fixpoint f) x

---------------------------------------------------------------------------------
--------                           PRUEBAS                               --------
---------------------------------------------------------------------------------
rotate1 = rotate [1,2,3]
-- regresa: [[1, 2, 3], [2, 3, 1], [3, 1, 2]]

remdups1 = remdups [1,2,2,3,3,3,1,1]
-- regresa: [1,2,3,1]
