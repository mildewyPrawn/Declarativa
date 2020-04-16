{-
- Programacion Declarativa 2020-1
- Tarea 4: I want to play a game
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Criptoalgoritmos where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map
import Data.List
import Data.Char
import Data.Maybe
--Funcion que indica si x es un Operador
--isOperator :: String -> Bool
isOperator x = elem x ["+","-","*","="]
--Funcion que regresa unas coasa bien lokas creo que esta es la que hay que
--modificar
reduceChar s = (words s, Data.List.map (myZip1 ms sr) (l sr))
  where (ms,sr) = (myZip2 (Data.List.filter isAlpha (prim $ words s)) [],
                    Data.List.filter (isAlpha) (nub s))
                  
myZip1 nV u p = if (Data.List.null (intersect nV g)) then g else []
  where g = myZip2 u p

prim [] = []
prim (xs:xss) = [head xs] ++ prim xss

-- otra madre que tarda menos
prim2 ll = Data.List.map (\x -> head x) ll

--l :: Foldable t => t a -> [String]
l s = Data.List.map show [0 .. (read (replicate (length s) '9') :: Int)]

--Funcion que dada una permutacion p, convierte la palabra w a los indices
--de p solo si w no es un operador, en ese caso regresa w sin cambios
convert1P p w = if isOperator w || p==[]
                then w
                else convert p2 w
  where p2 = fromList p

--Convierte una cadena a indices de p
--convert p (x:xs) = Prelude.foldr (\x xs p-> (p ! x) : xs) []
convert _ [] = []
convert p (x:xs) = (p ! x) : convert p xs

-- Otra madre 'minimizada' no sé porque no sé como probarla :v
convert2 p l = Data.List.map (\y -> (p ! y)) l

--Predicado para saber si jala, si la evaluación es igual al resutlado,
--entonces regresa True y False en caso contrario
jala ws [] = False
jala ws p = (==) (evalua1 0 (Data.List.take (fromJust $ elemIndex "=" l) l))
            (read (last l) :: Int)
  where l = Data.List.map (convert1P p) ws
  
--Evalúa una lista de cadenas para regresar un entero
--evalua1 :: Int -> [String] -> Int
evalua1 n [] = n
evalua1 n (x:xs)
  | isOperator x = evalua1 (evalua n x (head xs)) (tail xs)
  | otherwise = evalua1 (read x :: Int) xs
  
--Evalua un número entero y una cadena de numeros con el operador o
--evalua :: Int -> String -> String -> Int
evalua n1 o s2
   |o=="+" = n1 + n2
   |o=="-" = n1 - n2
   |o=="*" = n1 - n2
  where n2 = read s2 :: Int

--filtra :: [String] -> [String] -> [String]
filtra w pers = Data.List.filter (jala w) pers

--principal :: String -> [String]
principal s =  Set.fromList $ filtra w pers
  where (w , pers)= reduceChar s
  
--myZip :: String -> String -> [(Char, Char)]
myZip [] [] = []
myZip (a:as) [] = (a,'0') : myZip as []
myZip (a:as) (b:bs) = (a,b) : myZip as bs

myZip2 l@(x:xs) [] = Data.List.map (\y -> (y, '0')) l
myZip2 (a:as) (b:bs) = (a,b) : myZip as bs

-- Duda, tiene que ser siempre que l > r ??
myZip3 l r = zip l r ++ Data.List.map (\y -> (y, '0')) (Data.List.drop (length r)l)
