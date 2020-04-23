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

-- | binarios. Función que recibe una lista de números y regresa una lista de
--             números binarios asociados a los números orginales.
binarios :: [Int] -> [Int]
binarios xs = mapF (\x -> foldl toDigit 0 x) $ mapF (\x -> toBin x :: [Int]) xs

-- | triangulares. Función que recibe una lista de números y regresa una nueva
--                 que contiene únicamente aquellos que son triangulares.
-- Un número triangular es aquel que puede recomponerse en la forma de un
-- tríangulo equilátero.
-- triangulares :: [Int] -> [Int]
triangulares l = mapF (\x -> (round x) :: Int) $ filterF tP l

---------------------------------------------------------------------------------
--------                          AUXILIARES                             --------
---------------------------------------------------------------------------------
-- | toDigit. Función auxiliar para pasar listas a números
toDigit n d = 10*n + d

-- | tP. Función para ver si un número es triangular.
--       True si es un número triangular. False en otro caso.
tP n = (\x -> x == fromInteger (round x)) (sqrt(8*n + 1))

-- | mapF. Función map con foldr
mapF :: (a -> b) -> [a] -> [b]
mapF f [] = []
mapF f xs   = foldr (\y ys -> (f y):ys) [] xs

-- | reverseF. Función 'reverse' como instancia de foldr. (TAREA3)
reverseF :: [a] -> [a]
reverseF xs = foldr (\b g x -> g (b : x)) id xs []

-- | filterF. Función 'filter' como instancia de foldr. (TAREA3)
filterF :: (a -> Bool) -> [a] -> [a]
filterF _ [] = []
filterF p xs = foldr (\x xs -> if (p x) then x:xs else xs) [] xs

toBin 0 = [0]
toBin 1 = [1]
toBin n = toBin (div n 2) ++ [mod n 2]

---------------------------------------------------------------------------------
--------                           PRUEBAS                               --------
---------------------------------------------------------------------------------

factorion1 = factorion 145
-- Regresa: 145

iflip1 = iflip 1720
-- Regresa: 271

binarios1 = binarios [1..4]
-- Regresa: [0,1,10,11,100]

triangulares1 = triangulares [1..6]
-- Regresa: [1,3,6]
