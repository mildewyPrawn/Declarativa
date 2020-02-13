
{-
- Programacion Declarativa 2020-1
- Tarea 2: The Imperative is Dark and Full of Terrors
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Tarea2 where

import Data.List

--------------------------------------------------------------------------------
--------                        PRIMERA PARTE                           --------
--------------------------------------------------------------------------------

-- | gtrPower2. Función que encuentra la potencia de 2 más grande menor a n
gtrPower2 :: Int -> Int
gtrPower2 n = aux n 1
  where
    aux n m
      | n <= m*2 = m
      | otherwise = aux n (m*2)

-- | inarow. Función que recibe una lista de elementos comparables y regresa el
-- mayor número de apariciones consecutivas del mismo elemento.
-- inarow :: (Eq a) => [a] -> Int
inarow l = snd$head$sortBy compareTups (tupling l)
  where
    tupling [] = []
    tupling l =
      let
        prevMax = cuentaDist l (head l, 0)
      in
        [(head l, prevMax)]++(tupling$drop prevMax l)
    cuentaDist [] acc = snd acc
    cuentaDist (x:xs) acc = if x == fst acc -- (x, #)
                            then cuentaDist xs (x, (snd acc) + 1)
                            else snd acc

-- | ramanujan. Función que regresa una lista de tuplas de la forma
-- 0 < a,b,c,d <= n; y a3 + b3 = c3 + d3
ramanujan :: Int -> [(Int, Int, Int, Int)]
ramanujan = error "Falta"


-- | compareTups. Función que es un comparador, porque no supe llamarlo desde la
-- versión interactiva sin una función que compare.
compareTups :: (Ord a) => (a, Int) -> (a, Int) -> Ordering
compareTups (x, n) (y, m)
  | n < m = GT
  | m < n = LT
  | otherwise = compare x y
--------------------------------------------------------------------------------
--------                             Pruebas                            --------
--------------------------------------------------------------------------------

gtrPower21 = gtrPower2 3
-- regresa: 2

gtrPower22 = gtrPower2 8
-- regresa: 4

inarow1 = inarow "aabaaabbab"
-- regresa: 3

inarow2 = inarow [1,2,3,3,4,5]
-- regresa: 2
