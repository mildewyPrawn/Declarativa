module Proyecto where

import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Scale = Do | Reb | Re   | Mib |
             Mi | Fa  | Solb | Sol | 
             Lab| La  | Sib | Si deriving (Show )

type Order = Int
type Note = (Scale, Order)
type Notes = [Note]

-- ghc --make -dynamic music_parser.hs

notes :: Notes
notes = [(La, 9), (Si, 11), (Do, 0), (Re, 2), (Mi, 4), (Fa, 5), (Sol, 7),
         (Lab, 8), (Sib, 10), (Reb, 1), (Mib, 3), (Solb, 6)]

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
        Just (note, ord) -> [ord] ++ translate xs
        Nothing -> [] -- omite las letras que no están.
  else translate xs
