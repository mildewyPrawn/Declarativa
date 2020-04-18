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

data MyTup a =
  I a |
  Mul (MyTup a) (MyTup a) |
  Sum (MyTup a) (MyTup a) |
  Res (MyTup a) (MyTup a) |
  Iq (MyTup a) (MyTup a) deriving (Show, Eq)

-- data Op = Mul | Sum | Res deriving (Show)

type Identif = [Char]
type Sust = (Char, MyTup String)


-- resuelve :: MyTup String -> Int
resuelve (I a) = read a :: Int
-- resuelve (I a) = a
resuelve (Mul a b) = (resuelve a) * (resuelve b)
resuelve (Sum a b) = (resuelve a) + (resuelve b)
resuelve (Res a b) = (resuelve a) - (resuelve b)

-- verifica :: MyTup String -> Bool
verifica (Iq a b) = resuelve a == resuelve b

-- EJEMPLOS
-- verifica (Iq (Sum (I "333") (I "333")) (I "666"))
-- filter (/= I ' ') $ map (\x -> I x) "SEND + MORE = MONEY"

op = ["=","*","+","-"]

-- parseador [x] = I [x]
parseador s =
  let
    jni = elemIndex "=" s
  in
    case jni of
      Just x ->
        let
          (a,b) = Data.List.splitAt x s
        in
          Iq (parseador a) (parseador (tail b))
          -- Iq (parseador a) (parseador (tail b))
          -- Iq (parseador a) (read (tail b) :: Int)
      Nothing ->
        let
          jnm = elemIndex "*" s
        in
          case jnm of
            Just x ->
              let
                (a,b) = Data.List.splitAt x s
              in
                Mul (parseador a) (parseador (tail b))
            Nothing ->
              let
                jns = elemIndex "+" s
              in
                case jns of
                  Just x ->
                    let
                      (a,b) = Data.List.splitAt x s
                    in
                      Sum (parseador a) (parseador (tail b))
                  Nothing ->
                    let
                      jnr = elemIndex "-" s
                    in
                      case jnr of
                        Just x ->
                          let
                            (a,b) = Data.List.splitAt x s
                          in
                            Sum (parseador a) (parseador (tail b))
                        -- Nothing -> I (read (head s) :: Int)
                        Nothing -> I (head s :: String)

--Funcion que indica si x es un Operador
--isOperator :: String -> Bool
isOperator x = elem x ["+","-","*","="]
isOperatorC x = elem x ['+','-','*','=', ' ']
--Funcion que regresa unas coasa bien lokas creo que esta es la que hay que
--modificar

reduceChar s = (words s, Data.List.map (zip sr) (lV (Data.List.map (\x -> head x) $ words s) sr [""]))
  where sr = Data.List.filter (isAlpha) (nub s)


 --lV ms sr [""]

  --sr = SENDMORY
  --ms = NO VALIDAS
  -- where (ms,sr) = (myZip3 (Data.List.filter isAlpha (prim2 $ words s)) [],

lV :: (Foldable t, Eq t1) => t t1 -> [t1] -> [[Char]] -> [String]
lV nV [] act = act
lV nV (x:xs) act =  lV nV xs act2
        where act2 = if elem x nV
                     then [subs ++ [s] | s <- "123456789", subs <- act]
                     else [subs ++ [s] | s <- "0123456789", subs <- act]



--Data.List.map (\x -> 9) "asdfasdf"

--Funcion que dada una permutacion p, convierte la palabra w a los indices
--de p solo si w no es un operador, en ese caso regresa w sin cambios
convert1P p w = if isOperator w || p==[]
                then w
                else Data.List.map (\x -> (p2 ! x)) w
  where p2 = fromList p


jalaV s p= verifica $ parseador $ words $ conv p s


--Predicado para saber si jala, si la evaluación es igual al resutlado,
--entonces regresa True y False en caso contrario
jala ws p = (==) (evalua1 0 (Data.List.take (fromJust $ elemIndex "=" l) l))
            (read (last l) :: Int)
  where l = Data.List.map (convert1P p) ws

ent= "AA + BB = CC"


 {-[AA, +, BB, =, CC]
 [AA, +, BB]
 [(A,1) (B,1)(C,1)]-}

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

filtraV s p = Data.List.filter (jalaV s) p

--filtra :: [String] -> [String] -> [String]
filtra w pers = Data.List.filter (jala w) pers

--principal :: String -> [String]
principal s =  filtraV s pers
  where (w , pers)= reduceChar s

r="SEND + MORE = MONEY"

--conv :: [(String, String)] -> [Char] -> [Char]
conv p l = Data.List.map (\x -> if (isOperatorC x) then x else (p2 ! x)) l
                where p2 = fromList p

a = verifica $ parseador $ words $ conv [('A','1'),('B','2')] "AA + BB = AB"