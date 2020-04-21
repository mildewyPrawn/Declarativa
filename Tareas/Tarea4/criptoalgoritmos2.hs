{-
- Programacion Declarativa 2020-1
- Tarea 4: I want to play a game
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Criptoalgoritmos where
import Data.Map
import Data.List
import Data.Char
import Data.Maybe

--Funcion que indica si x es un Operador
isOperator :: String -> Bool
isOperator x = elem x ["+","-","*","="]
isOperatorC :: Char -> Bool
isOperatorC x = elem x ['+','-','*','=',' ']

--Funcion que dada una cadena que representa una operación valida para el
--programa, regresa una tupla la cuál tiene en su primer entrada
--una lsita con las palabras y operadores de la entrada.
--La segunda entrada tiene una lsita con todas las posibles soluciones del
--problema, por ejemplo si la entrada es AA + BB = CC, entonces esta entrada
--serían todas las listas de la forma [('A','n'),('B','m'),('C','o')]
reduceChar s = (words s,
                Data.List.map (zip sr) (lV (Data.List.map (\x -> head x) $ words s) sr [""]))
  where sr = Data.List.filter (isAlpha) (nub s)

--lV genera todas las posibles cadenas con las cuales zipear las letras de la
--entrada, por ejemplo si la entrada es AA + BB = CD, va a regresar una lsita
--con todas las posibles cadenas de la forma
--                      ABCD
--                      abcd
--Donde a, b y c pueden tomar valores del 1 al 9, mientras que d puede tomar
--del 0 al 9, con esto evitamos que ninguna letra que esté al principio de
--alguna palabra valga 0
lV :: (Foldable t, Eq t1) => t t1 -> [t1] -> [[Char]] -> [String]
lV nV [] act = act
lV nV (x:xs) act =  lV nV xs act2
        where act2 = if elem x nV
                     then [subs ++ [s] | s <- "123456789", subs <- act]
                     else [subs ++ [s] | s <- "0123456789", subs <- act]

--conv convierte una cadena a elementos del "Diccionario" p. por ejemplo
--si l = AA + BB = CC y p = [(A,'1'),(B,'2'),(C,'3')]
--conv regresará 11 + 22 = 33
conv :: [(Char, Char)] -> String -> String
conv p l = Data.List.map (\x -> if (isOperatorC x) then x else (p2 ! x)) l
                where p2 = fromList p


--Predicado para saber si una posible solucion lo es o no,
--si la evaluación es igual al resutlado, entonces regresa True y False en
--caso contrario
funciona :: [Char] -> Int -> [(Char, Char)] -> Bool
funciona ws n p = (==) (evalua1 0 (Data.List.take n l)) (read (last l) :: Int)
  where l = words $ conv p ws

--Evalúa una lista de cadenas para regresar un entero
evalua1 :: Int -> [String] -> Int
evalua1 n [] = n
evalua1 n (x:xs)
  | isOperator x = evalua1 (evalua n x (head xs)) (tail xs)
  | otherwise = evalua1 (read x :: Int) xs

--Evalua un número entero y una cadena de numeros con el operador o
evalua :: Int -> String -> String -> Int
evalua n1 o s2
   |o=="+" = n1 + n2
   |o=="-" = n1 - n2
   |o=="*" = n1 - n2
  where n2 = read s2 :: Int

--Filtra aplica filter con funciona para filtrar las soluciones correctas
--de toda la lista de soluciones posibles que genera lV.
filtra :: [String] -> [[(Char, Char)]] -> String -> [[(Char, Char)]]
filtra w pers s= Data.List.filter (funciona s n) pers
                    where n = fromJust $ elemIndex "=" w

--Funcion principal del programa.
criptoalgoritmos :: String -> [[(Char, Char)]]
criptoalgoritmos s =  filtra w pers s
  where (w , pers)= reduceChar s