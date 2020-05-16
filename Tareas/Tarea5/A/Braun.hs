{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Braun
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Braun where

 data BTree a = BVoid | BNode a (BTree a) (BTree a) deriving(Show)

 -- | isBal. Función que verifica si un árbol está balanceado.
 -- isBal :: BTree a -> Bool

 size :: BTree a -> Int
 size BVoid = 0
 size (BNode a t1 t2)=1 + size t1 + size t2

 --Arbol de ejemplo
 a :: BTree Int
 a = (BNode 0
      (BNode 1 (BNode 3 BVoid BVoid) (BNode 5 BVoid BVoid))
      (BNode 2 (BNode 4 BVoid BVoid) (BNode 6 BVoid BVoid)) )

 --Función que dado un árbol y un índice, regresa el elemento i del árbol cómo
 --si este fuera un arreglo. Considerando que los índices pares están a la
 --derecha y los impares a la izquerda.
 get (BNode r i d) 0 = r
 get (BNode r iz d) i = if even i then getP d (auxP i) 2
                       else  getI (BNode r iz d) (auxI i) 0

 --Métodos auxiliares del método get, necesarios para acceder elementos impares
 getI BVoid _ _ = error "NOMAMES"
 getI (BNode v d i) [] n = v
 getI (BNode v d i) (x:xs) n = if n*2+1 == x
                               then getI d xs x
                               else getI i xs x

 auxI n = reverse $ auxI2 [n] n

 auxI2 :: [Int] -> Int -> [Int]
 auxI2 l 1 = l
 auxI2 l n = auxI2 (l++[n0])  n0
            where n0 = myDiv n

 myDiv n = if even n2 then n2 - 1 else n2
            where n2= div n 2

 --Métodos auxiliares del método get, necesarios para acceder elementos pares
 getP BVoid _ _ = error "NOMAMES"
 getP (BNode v i d) [] n = v
 getP (BNode v i d) (x:xs) n = if n*2 == x
                               then getP i xs x
                               else getP d xs x

 auxP n = tail $ reverse $ auxP2 [n] n

 auxP2 l 2 = l
 auxP2 l n = auxP2 (l++[n0]) n0
            where n0 = myDiv2 n

 myDiv2 n = if even n2 then n2 else n2-1
            where n2 = div n 2


 update :: BTree a -> a -> Int -> BTree a
 update (BNode e i d) it 0 = (BNode it i d)
 update (BNode e i d) it ind = if even ind
           then (BNode e i (updateAuxP d it (auxP ind) 2))
           else updateAuxI (BNode e i d) it (auxI ind) 0

 updateAuxP :: BTree a -> a -> [Int] -> Int -> BTree a
 updateAuxP BVoid _ _ _ = error "NOMAMES"
 updateAuxP (BNode e i d) it [] _ = (BNode it i d)
 updateAuxP (BNode e i d) it (x:xs) n = if n*2 == x
                                        then (BNode e (updateAuxP i it xs x) d)
                                        else (BNode e i (updateAuxP d it xs x))

 updateAuxI :: BTree a -> a -> [Int] -> Int -> BTree a
 updateAuxI BVoid _ _ _= error "NOMAMES"
 updateAuxI (BNode v i d) it [] n = (BNode it i d)
 updateAuxI (BNode v i d) it (x:xs) n = if n *2+1 == x
                                        then (BNode v (updateAuxI i it xs x) d)
                                        else (BNode v i (updateAuxI d it xs x))

 toList :: BTree a -> [a]
 toList a = toListAux a [] (n-1)
            where n = size a

 toListAux :: BTree a -> [a] -> Int -> [a]
 toListAux (BNode v i d) l 0 = [v] ++ l
 toListAux a l n = toListAux a ([get a n]++l) (n-1)
