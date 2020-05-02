{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Braun
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Braun

data BTree a = BVoid | BNode a (Btree a) (Btree a)

-- | isBal. Función que verifica si un árbol está balanceado.
isBal :: BTree a -> Bool

