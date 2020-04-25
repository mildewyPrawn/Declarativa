{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Parte Extra
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Cartesianos where

import Data.Maybe
import Data.List

data CartT a = Void | Node (CartT a) a (CartT a) -- deriving (Show)

instance Show a => Show (CartT a) where
  show t = showSubL t ""

-- | cart. Función que construye un árbol cartesiano a partir de una lista
--         siguiendo
-- cart :: (Ord a) => [a] -> CartT a
cart []  = Void
cart [x] = Node Void x Void
cart xs  =
  let
    n = elemIndex (foldr min (head xs) xs) xs
  in
    case n of
      Just x ->
        let
          (a,b) = splitAt x xs
        in
          Node (cart a) (b !! 0) (cart $ drop 1 b)
      Nothing -> Void

-- | inorder. Función que regresa una lista con los elementos de un árbol
--            haciendo un recorrido in-order.
inorder :: CartT a -> [a]
inorder Void         = []
inorder (Node l n r) = inorder l ++ [n] ++ inorder r

-- | showSubL. Función que hace un 'prettyPrint' de los árboles.
showSubL Void s = s ++ "└─── V"
showSubL (Node v1@Void n v2@Void) s = show n ++
                                      nS ++ "├──(L) V" ++
                                      nS ++ "└──(R) V"
  where nS = "\n" ++ s
showSubL (Node Void n r) s          = show n ++
                                      nS ++ "├──(L) V" ++
                                      nS ++ "└──(R) " ++ showSubL r oS
  where
    nS = "\n" ++ s
    oS = addWhite 12 s
showSubL (Node l n Void) s          = show n ++
                                      nS ++ "├──(L) " ++ showSubL l oS ++ 
                                      nS ++ "└──(R) V"
  where
    nS = "\n" ++ s
    oS = addWhite 12 s
showSubL (Node l n r) s             = show n ++
                                      nS ++ "├──(L) " ++ showSubL l oS ++
                                      nS ++ "└──(R) " ++ showSubL r oS
  where
    nS = "\n" ++ s
    oS = addWhite 12 s



-- Auxiliar
addWhite 0 s = s
addWhite n s = if (n < 0)
               then s
               else addWhite (n-3) s ++ " "

-- Árboles 
siete = Node (Node (Node Void 4 Void) 2 (Node Void 5 Void)) 1 (Node (Node Void 6 Void) 3 (Node Void 7 Void))
tresI = (Node (Node (Node Void 3 Void)2 Void) 1 Void)
tresD = (Node Void 3 (Node Void 2 (Node Void 1 Void)))
