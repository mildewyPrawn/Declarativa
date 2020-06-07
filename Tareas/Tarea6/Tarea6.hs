{-
- Programacion Declarativa 2020-1
- Tarea 6: Release the Monad!
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Tarea6 where
 import Control.Monad
 import Data.List
 -- | timeTravel, calcula los posibles años dado un número de saltos de una
 -- | máquina del tiempo defectuosa
 timeTravel :: Int -> Int -> [Int]
 timeTravel 0 a = [a]
 timeTravel s a = [-1,3,5] >>= \p -> timeTravel (s-1) a >>= \r -> return (p+r)

 -- | timeTravelD, version con notacion do de timeTravel
 timeTravelD :: Int -> Int -> [Int]
 timeTravelD 0 a = [a]
 timeTravelD s a = do
                  p <- [-1,3,5]
                  r <- timeTravelD (s-1) a
                  return (p+r)


 primosHasta m = criba [2..m]
             where
             criba (x:xs) = x : criba (xs \\ [x,x+x..m])
             criba [] = []

 goldbach n = (primosHasta n)>>=
            \f1 -> (primosHasta n) >>=
            \f2 -> (primosHasta n) >>=
            \f3 -> guard(f1+f2+f3==n) >> return (f1,f2,f3)

 goldbachD :: Int -> [(Int, Int, Int)]
 goldbachD n = do
             f1 <- primosHasta n
             f2 <- primosHasta n
             f3 <- primosHasta n
             guard (f1+f2+f3 == n)
             return (f1,f2,f3)







