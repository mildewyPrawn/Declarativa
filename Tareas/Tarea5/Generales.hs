{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Generales
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Generales where

data Gtree a = Void | Node a [Gtree a] deriving (Show)
-- agregar Void para tener el árbol vacío, sino tenemos un árbol de al menos un
-- elemento

data BT a = VoidBT | NodeBT (BT a) a (BT a) deriving (Show)

-- | size. Función que calcula el tamaño (número de elementos) de un árbol
size :: Gtree a -> Int
size Void            = 0
size (Node n [])     = 1
size (Node n (x:xs)) = size x + size (Node n xs)

-- | depth. Función que calcula la profundidad (número de niveles) de un árbol.
depth :: Gtree a -> Int
depth = error ""

-- | tran. Función que transforma un árbol general en uno binario. No es
--         necesario que esté ordenado.
tran :: Gtree a -> BT a
tran = error ""

-- | mapg. Función de orden superior que está basada en map para listas.
mapg :: (a -> b) -> Gtree a -> Gtree b
mapg f (Void) = Void
mapg f (Node n [])     = Node (f n) []
mapg f (Node n (x:xs)) = Node (f n) (map (mapg f) (x:xs))

-- | foldg. Función de orden superior que está basada en fold para listas.
foldg :: (a -> [b] -> b) -> Gtree a -> b
foldg = error ""

-- | searchg. Función que verifica si un elemento pertenece al árbol general.
searchg :: (Eq a) => a -> Gtree a -> Bool
searchg e (Void)          = False
searchg e (Node n [])     = n == e
searchg e (Node n (x:xs)) = searchg e x || searchg e (Node n xs)


t3 = (Node 3 [Void])
t2 = (Node 3 [Node 2 [], Node 1 []])
t1 = (Node 3 [Node 2 [Node 3 [Node 1 [], Node 2 []]], Node 1 []])
