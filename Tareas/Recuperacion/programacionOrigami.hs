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
  where fib 1 = 1
        fib n = n*fib(n-1)

-- | iflip. Función que toma un número natural n y regresa el número construido
--          con los dígitos en orden inverso.
iflip :: Int -> Int
iflip i = foldl toDigit 0 $ reverseF $ mapF (\x -> read [x] :: Int) (show i)
  where toDigit n d = 10*n + d

-- | binarios. Función que recibe una lista de números y regresa una lista de
--             números binarios asociados a los números orginales.
binarios :: [Int] -> [Int]
binarios = error ""

-- | triangulares. Función que recibe una lista de números y regresa una nueva
--                 que contiene únicamente aquellos que son triangulares.
-- Un número triangular es aquel que puede recomponerse en la forma de un
-- tríangulo equilátero.
-- triangulares :: [Int] -> [Int]
triangulares l = mapF (\x -> round x) $ filterF tP l



t n = div (n*(n+1)) 2

tP n = (\x -> x == fromInteger (round x)) (sqrt(8*n + 1))

mapF f [] = []
mapF f xs   = foldr (\y ys -> (f y):ys) [] xs

reverseF :: [a] -> [a]
reverseF xs = foldr (\b g x -> g (b : x)) id xs []

filterF :: (a -> Bool) -> [a] -> [a]
filterF _ [] = []
filterF p xs = foldr (\x xs -> if (p x) then x:xs else xs) [] xs
