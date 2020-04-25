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
  show t = showTree t ""

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

desc Void = 0
desc (Node l n r) = 1 + desc l + desc r

-- showTree Void c = ""
-- showTree (Node Void n Void) c = c ++ show n
showTree (Node l n r) c =
  let
    s = addWhite 3 " "
    ll = desc l
    lr = desc r
  in
  c ++ show n ++ showSubL l s ++ showSubL r s
  -- "\n│"
  -- "\n└──"  ++ " " ++ showTree r t


-- Hasta ahorita funciona vergas con árboles completos
showSubL Void s               = s ++ "\n└─── V"
showSubL (Node Void n Void) s = "\n│" ++ s ++ "\n└─── " ++ show n ++ "\n" ++
                                nS ++ "└─── V" ++ "\n" ++ nS ++ "└─── V"
  where nS = s ++ addWhite 2 " "
-- showSubL (Node l n Void) s    = s ++ "\n└─── " ++ show n ++ showSubL l nS
  -- where nS = s ++ addWhite 2 " "
-- showSubl (Node Void n r)
showSubL (Node l n r) s = s ++ "\n├─── " ++ show n ++ showSubL l nS ++ showSubL r nS
  where nS = s ++ addWhite 2 " "
-- showSub t s = s

addWhite 0 c = c
addWhite n c = if (n < 0)
               then c
               else addWhite (n-3) c ++ " "


siete = Node (Node (Node Void 4 Void) 2 (Node Void 5 Void)) 1 (Node (Node Void 6 Void) 3 (Node Void 7 Void))
tresI = (Node (Node (Node Void 3 Void)2 Void) 1 Void)
