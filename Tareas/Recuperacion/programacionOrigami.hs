{-
- Programacion Declarativa 2020-1
- Tarea Recuperación: Tarea de reposición
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module ProgramacionOrigami where
-- usando foldr o foldl

-- | factorion. Función que calcula el factorion de un número.
-- El factorion se define como la suma de los factoriales de cada dígito del
-- número.
factorion :: Int -> Int
factorion i = foldr (\x y -> fib x + y) 0 (mapF (\x -> read [x] :: Int) (show i))

-- | iflip. Función que toma un número natural n y regresa el número construido
--          con los dígitos en orden inverso.
iflip :: Int -> Int
iflip = error ""

-- | binarios. Función que recibe una lista de números y regresa una lista de
--             números binarios asociados a los números orginales.
binarios :: [Int] -> [Int]
binarios = error ""

-- | triangulares. Función que recibe una lista de números y regresa una nueva
--                 que contiene únicamente aquellos que son triangulares.
-- Un número triangular es aquel que puede recomponerse en la forma de un
-- tríangulo equilátero.
triangulares :: [Int] -> [Int]
triangulares = error ""




fib 1 = 1
fib n = n*fib(n-1)

mapF f [] = []
mapF f xs   = foldr (\y ys -> (f y):ys) [] xs

reverseF :: [a] -> [a]
reverseF xs = foldr (\b g x -> g (b : x)) id xs []
