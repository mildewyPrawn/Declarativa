module MHBT where
 
 data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving(Show)

 -- Algoritmos para construir un árbol de altura mínima a partir de
 -- una lista de elementos

 -- Algoritmo recursivo 😍

 mhbtR :: [a] -> Tree a
 mhbtR [x] = Leaf x
 mhbtR xs = Fork (mhbtR ls) (mhbtR rs)
  where (ls,rs) = splitAt (div (length xs) 2) xs

 -- Algoritmo iterativo 🤮 
 mhbtI :: [a] -> Tree a
 mhbtI = head . iter . map Leaf 

 iter :: [Tree a] -> [Tree a]
 iter [] = []
 iter [x] = [x]
 iter (t1:t2:ts) = iter $ (Fork t1 t2):(iter ts)

 -- Alternativa con listas de enteros representando las alturas de los
 -- arboles

 -- Función de costo 
 cost :: Tree Int -> Int
 cost (Leaf x) = x 
 cost (Fork t1 t2) = 1 + max (cost t1) (cost t2)

 -- mincostTree :: [Int] -> Tree Int
 -- mincostTree = minBy cost . trees

 -- Función Trees que construye todos los posibles arboles dada una lista
 trees :: [Int] -> [Tree Int]
 trees [] = []
 trees [x] = [Leaf x]
 trees (x:xs) = concatMap (prefixes x) (trees xs)

 -- Prefixes define una lista de de todas las posibles maneras de agregar 
 -- x como hoja mas izquierda a un árbol
 prefixes :: a -> Tree a -> [Tree a]
 prefixes x t@(Leaf y) = [Fork (Leaf x) t]
 prefixes x t@(Fork u v) = [Fork (Leaf x) t] ++ 
                           [Fork u' v | u' <- prefixes x u]


