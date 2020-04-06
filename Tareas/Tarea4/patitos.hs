{-
- Programacion Declarativa 2020-1
- Tarea 4: I want to play a game
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Patitos where

import Data.List
import Data.List.Split   -- chunksO, splitOn
import Data.Char

type Generation = [[String]]


-- format :: Show a => [a] -> String
-- format = concat . zipWith (++) (cycle [" | "]) . map show

-- | printArray. Imprime la matriz 'bonito'
printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 1 arr

-- | genZero. Construye una generación inicial a partir de una cadena de entrada.
genZero :: String -> Generation
genZero xs =
  let
    tblr = map (filter (\x -> length x > 0)) (map (splitOn "") (splitOn "_" xs))
    n = length $ tblr !! 0
    m = length tblr
    bueno = filter (\x -> length x == n) tblr
  in
    if (not (n /= m) && bueno == tblr)
    then tblr
    else error "Pusiste un mal input \129414\128299"

-- evolution :: Generation -> Generation
-- evolution [] = []
evolution g@(x:xss) =
  let
    neigh = sort $ neighbours (multZ (zipper g 1 (length x)) 0)
  in
    -- neigh
    length g


zipper [] _ _ = []
zipper (x:xss) n m = [zip [n..m] x] ++ zipper xss (n) (m)

multZ [] _ = []
multZ (x:xss) n = [map (\y -> (fst y+n, snd y)) x] ++ multZ xss (n+length x)
-- https://stackoverflow.com/questions/11267637/neighbours-in-2dm-matrix-represented-as-list-of-list?fbclid=IwAR1oGKfuSO63LHGaYSGWIDhNrbU4wC2qJh_HlYOhgxm0phPBo9iKyiEd_0k

-- podemos usar esto con tuplas para poder identificarlos
rightList (x:y:rest) = (x,y) : rightList (y:rest)
rightList _ = []

right m = m >>= rightList

swap (x,y) = (y,x)
co direction = map swap . direction
left = co right

down = right . transpose
up   = co down

downRight (xs:ys:rest) = zip xs (drop 1 ys) ++ downRight (ys:rest)
downRight _            = []
upLeft = co downRight

upRight  = downRight . reverse
downLeft = co upRight

allDirections = [right, left, up, down,
                  downRight, upLeft, upRight, downLeft]

-- | neighbours. Función que da todos los vecinos de una posición en la matriz.
neighbours m = allDirections >>= ($m)
