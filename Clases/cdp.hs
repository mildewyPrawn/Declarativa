module Countdown where

 import Data.List
 
 data Expr = Num Int | App Op Expr Expr deriving(Eq)
 data Op = Add | Sub | Mul | Div deriving(Eq)
 type Value = Int

-- Instancia de Show para el tipo Expr
 instance Show Expr where
  show (Num i) = show i
  show (App op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

-- Instancia de Show para el tipo Op
 instance  Show Op where
  show Add = "+" 
  show Sub = "-" 
  show Mul = "*" 
  show Div = "/" 

 subseqs :: [a] -> [[a]]
 subseqs [x] = [[x]]
 subseqs (x:xs) = xss ++ [x]: map (x:) xss
  where xss = subseqs xs

 value :: Expr -> Value 
 value (Num x) = x
 value (App op e1 e2) = apply op (value e1) (value e2)

 apply :: Op -> (Value -> Value -> Value)
 apply Add = (+)
 apply Sub = (-)
 apply Mul = (*)
 apply Div = (div)

 -- legal :: Op -> Value -> Value -> Bool 
 -- legal Add v1 v2 = True
 -- legal Sub v1 v2 = v1 > v2
 -- legal Mul v1 v2 = True 
 -- legal Div v1 v2 = v2 >= 1 && v1 `mod` v2 == 0

 legal :: Op -> Value -> Value -> Bool 
 legal Add v1 v2 = v1 <= v2
 legal Sub v1 v2 = v1 > v2
 legal Mul v1 v2 = 1 < v1 && v1 <= v2 
 legal Div v1 v2 = v2 > 1 && v1 `mod` v2 == 0

 mkExprs :: [Int] -> [(Expr, Value)]
 mkExprs [x] = [(Num x, x)]
 mkExprs xs = [ev | (ys,zs) <- unmerges xs, ev1 <- mkExprs ys, ev2 <- mkExprs zs, ev <- combine ev1 ev2]

 unmerges :: [a] -> [([a],[a])]
 unmerges [x,y] = [([x],[y]),([y],[x])]
 unmerges (x:xs) = [([x],xs),(xs,[x])] ++ concatMap (add x) (unmerges xs)
  where add x (ys, zs) = [(x:ys, zs), (ys, x:zs)]

 combine :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
 combine (e1,v1) (e2,v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
  where ops = [Add, Sub, Mul, Div]

 nearest :: Int -> [(Expr, Value)] -> (Expr, Value)
 nearest n ((e,v) : evs) = if d == 0 then (e,v) else  (search n d (e,v) evs) where d = abs (n - v)

 search :: Int -> Int -> (Expr, Value) -> [(Expr, Value)] -> (Expr, Value)
 search n d ev [] = ev
 search n d ev ((e,v): evs) 
  | d' == 0  = (e,v)
  | d' < d = search n d' (e,v) evs
  | d' >= d = search n d ev evs
  where d' = abs (n - v)

 countdown1 :: Int -> [Int] -> (Expr, Value)
 countdown1 n = nearest n . concatMap mkExprs . subseqs

-- Encuentra todas las expresiones que mas cercanas a n
 countdowns :: Int -> [Int] -> [(Expr,Value)]
 countdowns n xs  = filter (\x -> d == (abs $ n - (snd x))) $ concatMap mkExprs $ subseqs xs
  where n' = countdown1 n xs
        d = abs $ n - (snd n')

-- Imprime en un elegante formato las tuplas (Expr, Value)
 display :: (Expr,Value) -> String
 display (e,v) = show e ++ " = " ++ show v
