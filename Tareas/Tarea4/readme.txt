Programacion Declarativa 2020-1
Tarea 4: I want to play a game
Profesor: Dr. Favio Ezequiel Miranda Perea
Ayudante: Javier EnriquezMendoza
Alumnos: Juan Alfonso Garduño Solís Emiliano Galeana Araujo

---------------------------------------------------------------------------------
---                        EJERCICIO 1: CRIPTOALGORITMOS                      ---
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
---                    EJERCICIO 2: JUEGO DE LA VIDA (PATITOS)                ---
---------------------------------------------------------------------------------

El programa está escrito en Haskell, elegimos este lenguaje, porque no pudimos
preparar los datos del input con 'Prolog', y porque en 'Lisp' se nos complicó
hacer format de las cadenas. Entonces regresams al siempre confiable Haskell.

El programa se puede ejecutar con ghci:

$ ghci patitos.hs

lo que nos llevará al interpŕete de Haskell. Las funciones (El nombre)son las
mismas que en el PDF, 'genZero', 'evolution', 'generations'.
Si se quiere ver la matriz de una manera más visual, se puede hacer

patitos > printArray $ genZero "BGB_GBG_BGB"

o pasando alguna variable de tipo Generation (no sirve en generations).

Nos ayudamos de una función con mónadas que encontramos en stackOverflow
(adjuntamos el link). Porque estuvo más entendible que lo que estábamos
intentando para sacar los vecinos de una celda.

---------------------------------------------------------------------------------
---                              EJERCICIO 1: CÁNTAROS                        ---
---------------------------------------------------------------------------------
