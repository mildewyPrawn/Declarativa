module Common where

import Data.Char
import Data.List.Split
import Merge

type Texto = String
type Palabra = String
type Run = (Int,Palabra)

t1 :: Texto
t1 = "Entonces se corrompen la fe y las buenas costumbres, se ensalza a los aduladores y a los pérfidos y triunfan los adversarios, por lo que se dispensa a su cólera, y porque los que poseen el imperio se hacen sectarios de aquellas doctrinas de que ellos se declararon intérpretes; de donde nace que se atrevan a usurpar el derecho y la autoridad de éstos y no enrojezcan, al vanagloriarse, de que ellos son inmediatamente elegidos por Dios y sus divinos decretos, y puramente humanas, al contrario, las potestades soberanas, a quienes, por lo tanto, quieren obligar con los decretos divinos, es decir, con sus decretos: nadie puede ignorar cuánto repugnan todas estas cosas a la felicidad del estado."

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort sm) ++ x:(qsort gt)
 where sm = [y | y <- xs, y < x]
       gt = [y | y <- xs, x <= y]

-- words :: Texto -> [Palabra]
-- words = splitOn " "

sortWords :: [Palabra] -> [Palabra]
sortWords = qsort

countRuns :: [Palabra] -> [Run]
countRuns [] = []
countRuns (x:xs) = ((lstIndex x xs) + 1, x): countRuns [y | y <- xs, y /= x]

lstIndex :: Palabra -> [Palabra] -> Int
lstIndex _ [] = 0
lstIndex p (x:xs)
 | p == x = 1 + lstIndex p xs
 | otherwise = 0

compareRuns :: Run -> Run -> Ordering
compareRuns (i,p) (j,q)
 | i < j = GT
 | j < i = LT 
 | otherwise = compare p q

sortRuns :: [Run] -> [Run]
sortRuns = mergeSortCon compareRuns

showRun :: Run -> String
showRun (i,p) = p ++ ": " ++ show(i) ++ " \n "

commonWords :: Int -> Texto -> String
commonWords n = concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map toLower
