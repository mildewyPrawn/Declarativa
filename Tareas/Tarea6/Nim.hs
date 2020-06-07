{-
- Programacion Declarativa 2020-1
- Tarea 6: Release the Monad! | juego de Nim
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

{-# LANGUAGE ScopedTypeVariables #-} -- para el r :: Int <- ...

module Nim where

import Text.Printf
import System.IO
import Control.Monad

-- | game_list. Tablero inicial del juego de nim.
game_list :: [[Char]]
game_list = [['*'],['*', '*'],['*', '*', '*'],['*', '*', '*', '*']]

-- | pp. Para formatear las cadenas bonito (pretty print)
pp :: Int -> String -> String
pp i x = printf "%d: %s" i x

-- | printList. Imprime el tablero en curso. Recibe el tablero
printList :: [[Char]] -> String
printList xs =
  let
    l = zip [0..] xs
  in
    unlines $ map (\x -> pp (fst x) (snd x)) l

-- | nim. Juego de Nim.
nim :: IO()
nim = initNim game_list True

-- | initNim. Incializa todo. y juega hasta que se acabe el juego
initNim :: [[Char]] -> Bool -> IO()
initNim l b = do
  let j = choose b -- el jugador en curso
  if null l -- si ya no hay tablero
    then
    do
      printf "\nGAME OVER .\n"
      printf "\nEl ganador es el jugador%d\n\n" j
    else
    do
      -- por alguna extraña y enferma razón, si uso printf se queja :|||||
      putStrLn "\nEl juego sigue."
      printf "\nTurno del jugador%d\n\n" j
      putStr $ printList l
      -- leer las entradas
      -- TODO: preguntar bonito: renglones> |usuario|
      print "renglon> "
      r :: Int  <- readLn
      print "cantidad> "
      n :: Int  <- readLn
      let t = quita l r n -- nueva jugada
      putStr $ printList t -- pp tablero
      initNim t (not b) -- recursión

-- | choose. Elije al jugador en curso, es solo para imprimri bonito.
choose :: Bool -> Int
choose True  = 1
choose False = 2
  
-- | quita. Función que hace una tirada. Recibe el tablero, el renglón y la
--          columna, valida que sea un renglón válido. Que el número de cerillos
--          sea válido y updatea ese renglón
quita :: [[Char]] -> Int -> Int -> [[Char]]
quita l r n = if r < length l
              then
                let
                  renglon = l !! r -- agarrar el r-ésimo renglón
                in
                  if n <= length renglon && n /= 0 -- validacion de cerillos
                  then
                    let
                      new_l = replaceI r (drop n renglon) l -- updateo
                    in
                      deleteAllI "" new_l -- quitar "" del tablero (no sirven)
                      -- TODO: While opcion valida 
                  else error "Elegiste más cerillos de los que se pueden o ninguno"
              else
                error "El índice es muy largo"

-- | deleteI. Función que borra un elemento de una lista dado un índice.
deleteI :: Int -> [a] -> [a]
deleteI _ []     = []
deleteI 0 (x:xs) = xs
deleteI n (x:xs) = if n < length (x:xs)
                   then [x] ++ deleteI (n-1) xs
                   else error "Número muy grande."

-- | addI. Función que agrega un elemento a una lista dado un índice.
addI :: Int -> a -> [a] -> [a]
addI 0 e xs       = [e] ++ xs
addI n e l@(x:xs) = if n > length l
                    then l ++ [e]
                    else [x] ++ addI (n-1) e xs

-- | replaceI. Función que updatea un renglón (borra y agrega).
replaceI :: Int -> a -> [a] -> [a]
replaceI n e l = addI n e (deleteI n l)

-- | deleteAllI. Función que borra un elemento de toda la lista.
deleteAllI :: (Eq a) => a -> [a] -> [a]
deleteAllI _ []     = []
deleteAllI o (x:xs) = if x == o
                      then deleteAllI o xs
                      else [x] ++ deleteAllI o xs
