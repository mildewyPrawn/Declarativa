module Proyecto where

import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Escala musical, notas hechas con quintas.
data Scale = Do | Reb | Re   | Mib |
             Mi | Fa  | Solb | Sol | 
             Lab| La  | Sib | Si deriving (Show )

type Order = Float -- El orden de la escala.
type Note = (Scale, Order) -- Una nota está dada por su nombre y el orden en la escala.
type Notes = [Note] -- Las notas son un conjunto de nota

-- ghc --make -dynamic music_parser.hs

notes :: Notes
notes = [(La, 9.0), (Si, 11.0), (Do, 0.0), (Re, 2.0), (Mi, 4.0), (Fa, 5.0),
         (Sol, 7.0),(Lab, 8.0), (Sib, 10.0), (Reb, 1.0), (Mib, 3.0), (Solb, 6.0)]

-- https://lotz84.github.io/haskellbyexample/ex/maps
mapi :: Map Char Note
mapi = Map.fromList $ zip ['A'..] notes

translate :: String -> [Order]
translate [] = []
translate (x:xs) =
  if isAlpha x
  then 
    let
      posible_note = Map.lookup x mapi
    in
      case posible_note of
        Just (note, ord) -> [(ord + 40)] ++ translate xs
        Nothing -> [] -- omite las letras que no están.
  else translate xs
