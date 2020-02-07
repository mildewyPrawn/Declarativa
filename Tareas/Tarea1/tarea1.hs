module Tarea1 where

import Data.Char
import Data.List
-- https://stackoverflow.com/questions/7141229/how-to-write-recursive-lambda-expression-in-haskell/7141261
--------------------------------------------------------------------------------
--------                        PRIMERA PARTE                           --------
--------------------------------------------------------------------------------

-- | quitaMayusculas. Función que elimina todas las mayúsculas de una cadena
quitaMayusculas :: String -> String
quitaMayusculas str = [ x | x <- str, not$isUpper x]

-- | soloLetras. Función que elimina todos los carácteres que no sean letras de
-- una cadena.
soloLetras :: String -> String
soloLetras str = [ x | x <- str, isLetter x]

-- | prefijo xs ys. Función que regresa un Bool. True si xs es prefijo de ys y
-- False en otro caso.
prefijo :: String -> String -> Bool
prefijo xs ys = head [x | x <- [isPrefixOf xs ys]]
--------------------------------------------------------------------------------
--------                        SEGUNDA PARTE                           --------
--------------------------------------------------------------------------------

-- | mergeSort. Algoritmo de ordenamiento.
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mezcla (mergeSort f) (mergeSort s)
  where (f,s) = parte xs

-- | Algoritmos auxiliares para mergeSort
parte :: [a] -> ([a], [a])
parte l =
  let m = div (length l) 2
  in
    (take m l, drop m l)

mezcla :: (Ord a) => [a] -> [a] -> [a]
mezcla f [] = f
mezcla [] s = s
mezcla f@(x:xs) s@(y:ys) = if (x < y)
                           then [x]++(mezcla xs s)
                           else [y]++(mezcla f ys)

-- | mezclaCon. Función de ordenamiento que recibe un comparador.
mezclaCon ::(a -> a -> Ordering) -> [a] -> [a] -> [a]
mezclaCon compare f [] = f
mezclaCon compare [] s = s
mezclaCon compare f@(x:xs) s@(y:ys)
    | compare x y == LT = [x]++(mezclaCon compare xs s)
    | otherwise = [y]++(mezclaCon compare f ys)

-- | mergeSortCon. Función auxiliar de mergeSortCon que mergea listas dependiendo
-- del comparador.
mergeSortCon :: (a -> a -> Ordering) -> [a] -> [a]
mergeSortCon compare [] = []
mergeSortCon compare [x] = [x]
mergeSortCon compare xs =
  mezclaCon compare (mergeSortCon compare f) (mergeSortCon compare s)
  where
    (f,s) = parte xs

-- | compareNums. Función que es un comparador, porque no supe llamarlo desde la
-- versión interactiva sin una función que compare.
compareNums :: Int -> Int -> Ordering
compareNums a b
  | a > b = GT
  | b > a = LT
  | otherwise = compare a b

--------------------------------------------------------------------------------
--------                        TERCERA PARTE                           --------
--------------------------------------------------------------------------------

data Color = Rojo | Amarillo | Verde | Azul deriving (Eq, Show)

data Balcanes =
  Albania | Bulgaria | BosniayHerzegovina |
  Kosovo | Macedonia | Montenegro deriving (Eq, Show)

type Ady = [(Balcanes, Balcanes)]

-- Adyacencias definidas.
adyacencias :: Ady
adyacencias =
  [ (Albania, Montenegro), (Albania, Kosovo), (Albania, Macedonia),
    (Bulgaria, Macedonia), (BosniayHerzegovina, Montenegro),
    (Kosovo, Macedonia), (Kosovo, Montenegro)
  ]

type Coloracion = [(Color, Balcanes)]

-- | esBuena. Función que dada una lista de adyacencias y una coloración, nos
-- dice si es una buena coloración o no.
esBuena :: Ady -> Coloracion -> Bool
esBuena ad co =
  let
    colores = [[ snd x | x <- co, fst x == Rojo],
               [ snd x | x <- co, fst x == Amarillo],
               [ snd x | x <- co, fst x == Verde],
               [ snd x | x <- co, fst x == Azul]]
  in
    verifica ad (pares$filter (( > 1) . length) colores)
  where
    pares [] = []
    pares (z:zs) = [ (x,y) | x <- z, y <- z, y /= x] ++ pares zs

-- | verifica. Función auxiliar que verifica que ciertas adyacencias no se
-- encuentren en la lista de adyacencias definida.
verifica :: Ady -> [(Balcanes, Balcanes)] -> Bool
verifica ad [] = True
verifica ad (x:xs)
  | elem x ad = False
  | otherwise = verifica ad xs

coloraciones :: Ady -> [Coloracion]
coloraciones = error "HOLA"


colores = [Rojo, Amarillo, Verde, Azul]
balcanes = [Albania, Bulgaria, BosniayHerzegovina, Kosovo, Macedonia, Montenegro]

todas = [ (x,y) | x <- colores, y <- balcanes]

--------------------------------------------------------------------------------
--------                          COLORACIONES                          --------
--------------------------------------------------------------------------------
coloracionBuena = [(Rojo, Albania), (Amarillo, Montenegro), (Verde, Macedonia),
                   (Verde, BosniayHerzegovina), (Azul, Bulgaria), (Azul, Kosovo)]
otraBuena = [(Rojo, BosniayHerzegovina), (Verde, Montenegro), (Amarillo, Kosovo),
             (Rojo, Albania), (Azul, Macedonia), (Rojo, Bulgaria)]

coloracionMala = [(Rojo, Albania), (Rojo, Montenegro), (Verde, Macedonia),
                  (Verde, BosniayHerzegovina), (Azul, Bulgaria), (Azul, Kosovo)]
--------------------------------------------------------------------------------
--------                             Pruebas                            --------
--------------------------------------------------------------------------------

quitaMayusculas1 = quitaMayusculas "I <3 Haskell"
-- regresa: "<3 askell"

soloLetras1 = soloLetras "Oppan Lambda Style!!!"
-- regresa: "OppanLambdaStyle"

prefijo1 = prefijo "Haskell" "Que chido es Haskell"
-- regresa: False

prefijo2 = prefijo "Que chido" "Que chido es Haskell"
-- regresa: True

mergeSort1 = mergeSort [2,4,6,8,0,1,3,5,7,9]
-- regresa: [0,1,2,3,4,5,6,7,8,9]

parte1 = parte [1,2,3,4]
-- regresa: ([1,2],[3,4])

parte2 = parte [1,2,3,4,5]
-- regresa: ([1,2],[3,4,5])

mezcla1 = mezcla [3,17] [4,18]
-- regresa: [3,4,17,18]
