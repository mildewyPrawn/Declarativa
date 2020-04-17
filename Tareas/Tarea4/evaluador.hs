module Evaluador where

import Data.Char
import Data.List
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
          (a,b) = splitAt x s
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
                (a,b) = splitAt x s
              in
                Mul (parseador a) (parseador (tail b))
            Nothing ->
              let
                jns = elemIndex "+" s
              in
                case jns of
                  Just x ->
                    let
                      (a,b) = splitAt x s
                    in
                      Sum (parseador a) (parseador (tail b))
                  Nothing ->
                    let
                      jnr = elemIndex "-" s
                    in
                      case jnr of
                        Just x ->
                          let
                            (a,b) = splitAt x s
                          in
                            Sum (parseador a) (parseador (tail b))
                        -- Nothing -> I (read (head s) :: Int)
                        Nothing -> I (head s :: String)
    -- tail b
    -- if head b == "="
    -- then Iq (parseador a) (parseador (tail b))
    -- else if head b == "+"
         -- then Sum (parseador a) (parseador (tail b))
         -- else if head b == "-"
              -- then Res (parseador a) (parseador (tail b))
              -- else Iq (parseador a) (parseador (tail b))

-- cambiador :: String -> MyTup String


-- sust [] l       = []

-- sust (x:xs) l = [x]


-- s = "SEND + MORE = MONEY"
s = "9567 + 1085 = 1O652"  


