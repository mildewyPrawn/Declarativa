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

-- | evolution. Función que dada una generación, nos regresa la siguiente.
evolution :: Generation -> Generation
evolution [] = []
evolution g@(x:xss) =
  let
    neigh = sort $ neighbours (multZ (zipper g 1 (length x)) 0)
  in
    retGen (map (\x -> snd x) (duckMutation (aplana neigh []) [])) (length g)

-- | generations. Función que dada una generación, nos regresa una lista de las
--                generaciones siguientes hasta que ya no se puedan generar.
generations :: Generation -> [Generation]
generations [] = error "\129414\129414\129414\129414, no hay suficientes \129414"
generations e =
  let
    e' = evolution e
  in
    if (e' == e)
    then [e']
    else [e] ++ generations e'

---------------------------------------------------------------------------------
--------                          AUXILIARES                             --------
---------------------------------------------------------------------------------

-- | printArray. Imprime la matriz 'bonito'
printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 1 arr

-- | retGen. Función que toma una lista, y regresa listas de listas de tamaño n.
retGen [] n = []
retGen l@(x:y:z:xs) n = [take n l]++retGen xs n

-- | duckMutation. Función que aplica las mutaciones necesarias a la siguiente
--                 generación. Se basa en las siguientes reglas:
-- Un pato bueno se mantiene bueno si es adyacente a exactamente dos o tres
-- patos también buenos, en otro caso se transforma en un pato malo.

-- Si un pato malo es adyacente a exactamente tres patos buenos entonces se
-- convierte en bueno, en otro caso sigue siendo malo.
-- |NOTA| La i-esima casilla de la siguiente generación va a estar dada por los
-- vecinos de la i-esima casilla en la generación actual.
duckMutation [] mut = mut
duckMutation ((c,ad):xs) mut = if (snd c) == "G"
                               then if (snd ad == 2 || snd ad == 3)
                                    then duckMutation xs (mut++[(fst c,"G")])
                                    else duckMutation xs (mut++[(fst c,"B")])
                               else if (snd ad == 3)
                                    then duckMutation xs (mut++[(fst c,"G")])
                                    else duckMutation xs (mut++[(fst c,"B")])

-- | aplana. Función que toma todos los vecinos de una casilla para todas las
--           casillas. Y regresa la casilla con el número de vecinos buenos y
--           malos.
aplana [] ac = ac
aplana ((a,n):xs) ac = if snd n == "B"
                       then aplana xs (agrega (a, (1,0)) ac)
                       else aplana xs (agrega (a, (0,1)) ac)

-- | agrega. Función dada una lista de tuplas, agrega una nueva si no existe, y
--           si existe, suma a los vecinos que tiene.
agrega (a,s) [] = [(a,s)]
agrega (a,s) (x:xs) = if a == fst x
                      then [(a, sumT s (snd x) )] ++ xs
                      else [x] ++ agrega (a,s) xs
  where sumT (a,b) (c,d) = (a+c, b+d)

-- | zipper. Función que le asigna las mismas coordenadas a las posiciones de
--           las listas.
zipper [] _ _ = []
zipper (x:xss) n m = [zip [n..m] x] ++ zipper xss (n) (m)

-- | multZ. Función que le aplica una 'profundidad' a las coordenadas.
multZ [] _ = []
multZ (x:xss) n = [map (\y -> (fst y+n, snd y)) x] ++ multZ xss (n+length x)


-- https://stackoverflow.com/questions/11267637/neighbours-in-2dm-matrix-represented-as-list-of-list?fbclid=IwAR1oGKfuSO63LHGaYSGWIDhNrbU4wC2qJh_HlYOhgxm0phPBo9iKyiEd_0k
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
