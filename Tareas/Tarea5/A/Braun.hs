{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Braun
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Braun where

 data BTree a = BVoid | BNode a (BTree a) (BTree a) deriving(Show,Eq)

 -- | isBal. Función que verifica si un árbol está balanceado.
 -- isBal :: BTree a -> Bool



 --Arbol de ejemplo
 a :: BTree Int
 a = (BNode 0
      (BNode 1 (BNode 3 BVoid BVoid) (BNode 5 BVoid BVoid))
      (BNode 2 (BNode 4 BVoid BVoid) (BNode 6 BVoid BVoid)) )

 {-
 Funcion que calcula el número de elementos de un árbol, se basa
 en la invariante de que los sub-arboles tienen el mismo número de
 elementos o bien el sub-árbol izquierdo tiene un elemento más,
 por eso hacemos 2*n + diferencia i n
 La función diferencia calcula la diferencia de elementos entre el
 sub-árbol izquierdo y el derecho
 -}
 size BVoid = 0
 size (BNode v i d) = 1 + 2 * n + diferencia i n
                        where n = size d

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






 {-
 Si tenemos el árbol:    a         Y buscamos agregarle          h
                        / \        h al principio, espe-        / \
                       b   c       raríamos tener el           a   b
                     / \  / \      siguiente resultado:      / \  / \
                    d  f e   g                              c  e d   f
                                                           /
                                                          g
 El que antes era el sub-árbol izquierdo se respeta y se convierte en el
 subárbol derecho, sentido porque todos los índices impares al agregar un
 elemento se vuelven pares y viceversa.
 Pero el que antes era el sub árbol derecho si tiene cambios, sin embargo
 podemos notar se repite el mismo comportamiento.

 Supongamos que          c       Espereamríamos                 a
 quisieramos agregar    / \      obtener el siguiente          / \
 a  al árbol:          e   g     resutlado:                   c   e
                                                            /
                                                           g
 Que justamente resulta ser el sub-árbol izquierdo del resultado del primer
 ejemplo con la misma propiedad de que el sub-árbol izquierdo se respeta y
 ahora se convierte en el sub-árbol derecho (Tal vez no se note tanto con un
 árbol tan pequeño pero es difícil dibujarlos aquí). Esto sugiere que podemos
 el  agregar al principio como un con constante "pon el item a agregar en la
 raíz,  cambia el sub árbol izquierdo al derecho, agrega la raíz antigua al
 sub-árbol  derecho que ahora va a ser el sub-arbol izquierdo. Continúa hasta
 que llegues a un nodo vacío."
 Cuando agregas un elemento a un árbol con un nodo
 más en el sub-arbol izquierdo (como los resultados de los ejemplos) pues va a
 pasar que ahora ese nodo va a irse al sub-arbol derecho y vamos a agregar
 otro nodo en el sub árbol derecho que ahora va a ser el izquierdo, haciendo
 que ahora ambos tengan el mismo número de nodos y así sucesivamente, solo hay
 dos casos y tompo mucho tiempo pensar en eso...
 -}
 lowExt :: BTree a -> a -> BTree a
 lowExt (BVoid) item = (BNode item BVoid BVoid)
 lowExt (BNode v i d) item = (BNode item (lowExt d v) i)


 {-
 Aquí usamos lo mismo que lowExt solo que ahora el que queda intacto es
 el sub-árbol derecho que pasa a ser el sub-árbol izquierdo, mientras que
 para hacer el subárbol derecho del resultado haremos casi lo mismo que en
 lowExt solo que vamos a ir desplazando el valor v que estaba en la raíz de
 manera que cuando lleguemos un nodo que ya es hoja, lo dejamos como hoja,
 es como si fueramos bajando la raíz hasta sacarla del árbol
 -}
 lowRem :: BTree a -> BTree a
 lowRem (BVoid) = BVoid
 lowRem (BNode v BVoid _) = BVoid
 lowRem (BNode v (BNode v' i' d') d) = (BNode v' d (lowRem(BNode v i' d')))


 highExt BVoid item = (BNode item BVoid BVoid)
 highExt (BNode v BVoid BVoid) item = (BNode v (BNode item BVoid BVoid) BVoid)
 highExt (BNode v i BVoid) item = (BNode v i (BNode item BVoid BVoid))
 highExt (BNode v i d) item = if dl == 1
                            then (BNode v i (highExt2A d item))
                            else (BNode v (highExt2A i item) d)
                            where dl = diferencia i (size d)

 highExt2A (BNode v i d) item = highExt2 (BNode v i d) item (size d)


 highExt2 (BVoid) item n = (BNode item BVoid BVoid)
 highExt2 (BNode v BVoid BVoid) item n =(BNode v (BNode item BVoid BVoid) BVoid)
 highExt2 (BNode v i BVoid) item n = (BNode v i (BNode item BVoid BVoid))
 highExt2 (BNode v i d) item n = if even n
                            then (BNode v i (highExt2 d item ((div n 2) -1)))
                            else  (BNode v (highExt2 i item (div n 2)) d)



 highRem (BNode v BVoid BVoid) = (BVoid)

 highRem (BNode v i d) = if dl == 1
                         then (BNode v (highRem i) d)
                         else (BNode v i (highRem d))
                         where dl = diferencia i (size d)


 fromList l = fromListAux (reverse l) (BVoid)

 fromListAux (x:xs) a = fromListAux xs (lowExt a x)
 fromListAux [] a = a

 copy :: a -> Int -> BTree a
 copy item n = copyAux item n (BVoid)

 copyAux :: a -> Int -> BTree a -> BTree a
 copyAux item 0 t = t
 copyAux item n t = copyAux item (n-1) (lowExt t item)

 ---------------------------------------------------------------
 --Funciones auxiliares

 diferencia BVoid 0 = 0
 diferencia (BNode v BVoid BVoid) 0 = 1
 diferencia (BNode _ i d) n = if even n
                              then diferencia d ((div n 2) -1)
                              else diferencia i (div n 2)