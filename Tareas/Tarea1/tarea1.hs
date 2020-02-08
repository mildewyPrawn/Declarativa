
-- Emiliano Galeana Araujo
-- 314032324
-- galeanaara@ciencias.unam.mx
-- práctica01 Programación declarativa

module Tarea1 where

import Data.Char
import Data.List

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
  Kosovo | Macedonia | Montenegro deriving (Eq, Show, Ord)

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
-- Saca listas de los paises adyacentes, si la lista tiene un elemento, es una
-- buena coloración. Y para las que tienen más elementos, saca las tuplas (x,y)
-- y (y,x).
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
    pares (z:zs) = [ (x,y) | x <- z, y <- z] ++ pares zs

-- | verifica. Función auxiliar que verifica que ciertas adyacencias no se
-- encuentren en la lista de adyacencias definida.
verifica :: Ady -> [(Balcanes, Balcanes)] -> Bool
verifica ad [] = True
verifica ad (x:xs)
  | elem x ad = False
  | otherwise = verifica ad xs

-- | Lista de todos los colores
colores :: [Color]
colores = [Rojo, Amarillo, Verde, Azul]

-- | Lista de todos los Balcanes
balcanes :: [Balcanes]
balcanes = [Albania, Bulgaria, BosniayHerzegovina, Kosovo, Macedonia, Montenegro]

-- | coloraciones. Función que obtiene todas las coloraciones dada una lista de
-- adyacencias. NOTA. genera bajo un orden, y no genera las permutaciones, por lo
-- que la lista [a,b,c,d,e,f] y la lista [a,b,c,d,f,e] se cuentan como una misma.
-- Donde a,b,c,d,e,f son tuplas de la forma (Color, Balcanes).
coloraciones :: Ady -> [Coloracion]
coloraciones ad = filter (esBuena ad) adi
  where
    adi = [[(c0,p0)] ++ [(c1,p1)] ++ [(c2,p2)]
            ++ [(c3,p3)] ++ [(c4,p4)] ++ [(c5,p5)]
          |
           c0 <- colores,
           c1 <- colores,
           c2 <- colores,
           c3 <- colores,
           c4 <- colores,
           c5 <- colores,
           p0 <- balcanes,
           p1 <- balcanes,
           p2 <- balcanes,
           p3 <- balcanes,
           p4 <- balcanes,
           p5 <- balcanes,
           p0 /= p1, p0 /= p2, p0 /= p3, p0 /= p4, p0 /= p5,
           p1 /= p2, p1 /= p3, p1 /= p4, p1 /= p5,
           p2 /= p3, p2 /= p4, p2 /= p5,
           p3 /= p4, p3 /= p5,
           p4 /= p5,
           p0 < p1, p0 < p2, p0 < p3, p0 < p4, p0 < p5,
           p1 < p2, p1 < p3, p1 < p4, p1 < p5,
           p2 < p3, p2 < p4, p2 < p5,
           p3 < p4, p3 < p5,
           p4 < p5]

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

mergeSortCon1 = mergeSortCon compareNums [3,4,5,6,1,5,7,8,2,45,6,7,1,4]
-- regresa: [1,1,2,3,4,4,5,5,6,6,7,7,8,45]

esBuena1 = esBuena adyacencias coloracionBuena
-- regresa: True

esBuena2 = esBuena adyacencias coloracionMala
-- regresa: False

coloraciones1 = coloraciones adyacencias
-- regresa una lista de 432 elementos, muy larga para ponerla aquí
