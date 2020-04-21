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
-- Solo usando map y filter


-- | todos. Función que decide si todos los elementos de una lista cumplen un
--          predicado
todos :: (a -> Bool) -> [a] -> Bool
todos p xs = foldr (&&) True $ map p xs

-- | alguno. Función que decide si al menos un elemento de una lista cumplen un
--           predicado
alguno :: (a -> Bool) -> [a] -> Bool
alguno p xs = foldr (||) True $ map p xs 

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

-- | altMap. Función que aplica alternadamente las funciones que recibe como
--           argumentos.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g l = aux f g True l
  where
    aux f g _ [] = []
    aux f g True (x:xs) = f x : aux f g False xs
    aux f g False (x:xs) = g x : aux f g True xs

luhn :: [Int] -> Bool
luhn l = (\x -> mod x 10 == 0) $ sum $ map (\x -> if x > 9 then x-9 else x) $
         altMap (*2) (*1) l

-- pruebas

l = [3,3,7,9,5,1,3,5,6,1,1,0,8,7,9,5]
