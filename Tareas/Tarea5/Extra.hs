{-
- Programacion Declarativa 2020-1
- Tarea 5: I'm Groot. Parte Extra
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Extra where

data Time = V Int Int | D Int Int Bool
-- True  si es PM.
-- False si es AM.

instance Show Time where
  show a = showTime a

instance Eq Time where
  a == b = eqTime a b

instance Ord Time where
  a `compare` b = ltTime a b  

-- | to24. Función que transforma una hora de 24 a 12 horas.
to24 :: Time -> Time
to24 (D h m b) = D h m b
to24 (V h m)   = if (h <= 12)
                 then D h m False
                 else if (h > 24)
                      then error "Hora no soportada"
                      else D (mod h 12) m True

-- | to12. Función que transforma una hora de 12 a 24 horas.
to12 :: Time -> Time
to12 (V h m)   = V h m
to12 (D h m b) = if b
                 then V (h+12) m
                 else V h m

-- | eqTime. Función que decide si dos horas son iguales, sin importar su
--           formato.
eqTime :: Time -> Time -> Bool
eqTime (V h1 m1) (V h2 m2)       = h1 == h2 && m1 == m2
eqTime (D h1 m1 b1) (D h2 m2 b2) = if b1 == b2
                                   then h1 == h2 && m1 == m2
                                   else False
eqTime d@(D h1 m1 b) v@(V h2 m2) = eqTime v d
eqTime (V h1 m1) (D h2 m2 b)     = if b
                                   then if h1 >= 12
                                        then (mod h1 12) == h2 && m1 == m2
                                        else False
                                   else if h1 < 12
                                        then h1 == h2 && m1 == m2
                                        else False

-- | showTime. Función que muestra en pantalla las horas de acuerdo al siguiente
--             formato:
--             Para 24 horas 'xxxxHRS'.
--             Para 12 horas 'hh:mmAM' o 'hh:mmPM', con excepción de las horas
--               12:00AM que deberá mostrar 'Medianoche' y 12:00PM que deberá
--               mostrar 'Mediodia'.
showTime :: Time -> String
showTime (V h m)        = show h ++ show m ++ "HRS"
showTime (D 12 0 True)  = "Mediodia"
showTime (D 12 0 False) = "Medianoche"
-- TODO: hay que arreglar que to24 (V 0 0)/(D 0 0 _) regrese algo bien
showTime (D h m b)      = show h ++ ":" ++ show m ++ if b then "PM" else "AM"

-- | ltTime. Función que decide si una hora es menor a otra, independientemente
--           del formato.
--           True si a < b
--           False en otro caso
-- ltTime :: Time -> Time -> Ordering
ltTime (V h1 m1) (V h2 m2)     = if h1 == h2
                                 then compare m1 m2
                                 else compare h1 h2
ltTime a@(D _ _ _) b@(D _ _ _) = ltTime (to12 a) (to12 b)
ltTime a@(V _ _) b@(D _ _ _)   = ltTime a (to12 b)
ltTime a@(D _ _ _) b@(V _ _)   = ltTime (to12 a) b

-- horas
-- stct = D "6:34PM"
stct = D 6 34 True
tdo = V 15 18
