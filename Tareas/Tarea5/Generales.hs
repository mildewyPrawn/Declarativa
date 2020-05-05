{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Generales
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Generales where

import Data.List

data Gtree a = Node a [Gtree a] deriving (Show)
-- agregar Void para tener el árbol vacío, sino tenemos un árbol de al menos un
-- elemento

data BT a = VoidBT | NodeBT (BT a) a (BT a) deriving (Show)

-- | size. Función que calcula el tamaño (número de elementos) de un árbol
size :: Gtree a -> Int
size (Node n [])     = 1
size (Node n (x:xs)) = size x + size (Node n xs)

-- | depth. Función que calcula la profundidad (número de niveles) de un árbol.
depth :: Gtree a -> Int
depth t = depthAux t 0

depthAux (Node n []) m = 1 + m
depthAux (Node n [x]) m =
  let
    m' = depthAux x 0
  in
    1 + max m m'
depthAux (Node n (x:y:ys)) m =
  let
    mx = depthAux x 0
    my = depthAux y 0
  in
    depthAux (Node n ys) (max (max mx my) m)


-- | tran. Función que transforma un árbol general en uno binario. No es
--         necesario que esté ordenado.
tran :: Gtree a -> BT a
tran = listToBin . genToList

-- | mapg. Función de orden superior que está basada en map para listas.
mapg :: (a -> b) -> Gtree a -> Gtree b
mapg f (Node n []) = Node (f n) []
mapg f (Node n xs) = Node (f n) (map (mapg f) xs)

-- | foldg. Función de orden superior que está basada en fold para listas.
-- foldg :: (a -> [b] -> b) -> Gtree a -> b
foldg :: (a -> b -> b) -> b -> Gtree a -> b
foldg f e t =
  let
    lt = genToList t
  in
    foldr f e lt

-------------------------------------------------------
foldg2 :: (a -> [b] -> b) -> Gtree a -> b 			 --
foldg2 f (Node n []) = f n []						 --
foldg2 f (Node n l) = f n (map (foldg2 f) l)		 --
													 --
mySum n [] = n 										 --
mySum n l = n + sum l 								 --
-------------------------------------------------------

-- | searchg. Función que verifica si un elemento pertenece al árbol general.
searchg :: (Eq a) => a -> Gtree a -> Bool
searchg e (Node n [])     = n == e
searchg e (Node n (x:xs)) = searchg e x || searchg e (Node n xs)

-- | genToList. Función que pasa un Gtree a una lista (lo aplana).
genToList :: Gtree a -> [a]
genToList (Node n [])     = [n]
genToList (Node n (x:xs)) = genToList x ++ genToList (Node n xs)

-- | listToBin. Función que pasa una lista a un árbol binario de tipo 1.
listToBin :: [a] -> BT a
listToBin l  =
  let
    (a,b) = splitAt (div (length l) 2) l
  in
    NodeBT (listToBin a) (b !! 0) (listToBin (drop 1 b))

t3 = (Node 3 [])
t2 = (Node 3 [Node 2 [], Node 1 []])
t1 = (Node 3 [Node 2 [Node 3 [Node 1 [], Node 2 []]], Node 1 []])
t4 = (Node 1 [Node 2 [Node 3 []]])
