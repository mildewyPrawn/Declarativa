{-
- Programacion Declarativa 2020-1
- Tarea Recuperación: Tarea de reposición
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module OrdenSuperior where

import Data.List
import Data.Maybe

---------------------------------------------------------------------------------
--------                        PRIMERA PARTE                           ---------
---------------------------------------------------------------------------------
-- Solo usando map y filter

-- | todos. Función que decide si todos los elementos de una lista cumplen un
--          predicado
todos :: (a -> Bool) -> [a] -> Bool
todos p xs = foldr (&&) True $ map p xs

-- | alguno. Función que decide si al menos un elemento de una lista cumplen un
--           predicado
alguno :: (a -> Bool) -> [a] -> Bool
alguno p xs = foldr (||) False $ map p xs

-- | toma. Función que selecciona elementos de una lista mientras cumplan un
--         predicado (equivalente a takeWhile del preludio).
toma :: (Eq a) => (a -> Bool) -> [a] -> [a]
toma p xs = take ((\x -> fromJust x) $ elemIndex
                   (head $ filter (\x -> not $ p x) xs) xs) xs


-- | deja. Función que elimina elementos de una lista mientras cumplan el
--         predicado.
deja :: (Eq a) => (a -> Bool) -> [a] -> [a]
deja p xs = drop ((\x -> fromJust x) $ elemIndex
                   (head $ filter (\x -> not $ p x) xs) xs) xs

---------------------------------------------------------------------------------
--------                        SEGUNDA PARTE                           ---------
---------------------------------------------------------------------------------

-- | altMap. Función que aplica alternadamente las funciones que recibe como
--           argumentos.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

---------------------------------------------------------------------------------
--------                        TERCERA PARTE                           ---------
---------------------------------------------------------------------------------

luhn :: [Int] -> Bool
luhn l = (\x -> mod x 10 == 0) $ sum $ map (\x -> if x > 9 then x-9 else x) $
         altMap (*2) (*1) l

---------------------------------------------------------------------------------
--------                           PRUEBAS                               --------
---------------------------------------------------------------------------------
l :: [Int]
l = [3,3,7,9,5,1,3,5,6,1,1,0,8,7,9,5]

-- Todos los elementos son iguales a 1.
todos1 = todos (1==) l
-- Regresa: False

-- Todos los elementos son mayores o iguales a 0.
todos2 = todos (>=0) l
-- Regresa: True

-- Algún elemento es igual a 1
alguno1 = alguno (1==) l
-- Regresa: True

-- Alguno es menor a 0.
alguno2 = alguno (<0) l
-- Regresa: False

-- Toma los elementos que son iguales a 3.
toma1 = toma (3==) l
-- Regresa: [3,3]

-- Elimina los elementos que son iguales a 3.
deja1 = deja (3==) l
-- Regresa: [7,9,5,1,3,5,6,1,1,0,8,7,9,5]

altMap1 = altMap (+10) (+100) [0..4]
-- Regresa: [10,101,12,103,14]

luhn1 = luhn l
-- Regresa: True
