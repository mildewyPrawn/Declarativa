module Huffman where 

 import Data.List
 
 data Btree a = Tip a | Node (Btree a) (Btree a) deriving(Show)

 data Step = Zero | One 
 instance Show Step where
 	show Zero = "0"
 	show One = "1" 

 type Path = [Step]

 type Freq = (Char, Int)

 trace :: Btree a -> Path -> a
 trace (Tip x) [] = x
 trace (Node t1 _) (Zero:ps) = trace t1 ps
 trace (Node _ t2) (One:ps) = trace t2 ps

 -- Preprocessing
 
 count :: (Eq a) => a -> [a] -> Int
 count _ [] = 0
 count e (x:xs) 
  | e == x = 1 + cxs
  | otherwise = cxs
  where cxs = count e xs

 pairify :: String -> [Freq]
 pairify str =  map (\x -> (x,count x str)) (nub str)
  
 preprocess :: String -> [Freq]
 preprocess = (sortBy sorting) . pairify 
  where sorting x y = compare (snd x) (snd y) 

 -- Constructing a Huffman tree

 single :: [a] -> Bool
 single [x] = True
 single _ = False
 
 -- combine debe combinar 2 árboles con el menor peso
 -- para garantizarlo, los árboles deben mantenerse 
 -- ordenados respecto a su peso

 weight :: Btree Freq -> Int
 weight (Tip (_,w)) = w
 weight (Node t1 t2) = weight t1 + weight t2

 combine :: [Btree Freq] -> [Btree Freq]
 combine (t1:t2:ts) = add (Node t1 t2) ts

 add :: Btree Freq -> [Btree Freq] -> [Btree Freq]
 add u [] = [u]
 add u (t:ts) 
  | weight u <= weight t = u:t:ts
  | otherwise = t:(add u ts)

 build :: [Freq] -> Btree Freq
 build xs = head $ until single combine (map Tip xs)

 -- Coding

 codes :: Btree Freq -> Char -> Path
 codes (Tip y) x = []
 codes (Node t1 t2) x
  | member x t1 = Zero:codes t1 x
  | member x t2 = One:codes t2 x
  | otherwise = error "No es un caracter valido"

 member ::  Char -> Btree Freq -> Bool
 member x (Tip y) = x == fst y
 member x (Node t1 t2) = (member x t1) || (member x t2)


 code :: Btree Freq -> String -> Path
 code t = concat . map (codes t)

 -- Decoding

 traces :: Btree Freq -> Btree Freq -> Path -> String
 traces t (Tip x) [] = [fst x]
 traces t (Tip x) ps = [fst x] ++ traces t t ps
 traces t (Node t1 _) (Zero:ps) = traces t t1 ps
 traces t (Node _ t2) (One:ps) = traces t t2 ps

 decode :: Btree Freq -> Path -> String
 decode t ps = traces t t ps