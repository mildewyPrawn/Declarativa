Programacion Declarativa 2020-1
Tarea 4: I want to play a game
Profesor: Dr. Favio Ezequiel Miranda Perea
Ayudante: Javier EnriquezMendoza
Alumnos: Juan Alfonso Garduño Solís Emiliano Galeana Araujo

---------------------------------------------------------------------------------
---                        EJERCICIO 1: CRIPTOALGORITMOS                      ---
---------------------------------------------------------------------------------
El programa está escrito en Haskell, lo elegimos porque fué sin duda el que más
trabajo nos costó y debíamos hacerlo en algo que conocieramos mejor, nuestro
más grande problema fué sin duda intentar bajar el tiempo de ejecucion
basandonos en el ejemplo del PDF "SEND + MORE = MONEY", sin embargo debemos
admitir que no lo logramos :(, para dicha entrada nuestro programa tarda
entre 30 y 25 minutos, pero lo hace bien xD.
Tiene una limitación, y es que se debe ingresar la operación asocioando a la
izquierda: N1 op N2 op N3 se evalúa ((N1 op N2) op N3), intentando resolver
esta limitacion terminamos haciendo que el programa también pueda resolver
entradas de la forma "AA + BB = CC + DD".

Para ejecutar el programa lo cargamos en el modo interactivo con:
$ ghci criptoalgoritmos.hs

Para resolver una entrada hacemos:

$ criptoalgoritmos "AA + BB = CC"

por ejemplo.
Ya que no pudimos resolver al menos en tiempo este ejercicio también incluimos
el archivo criptoalgoritmos2.hs, el cuál puede resolver entradas con
operaciones en ambos lados de la igualdad, como

$criptoalgoritmos2 "AA + BB = DD * C"

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
Para este ejercicio elegimos Haskell (quesorpresa), aquí las listas por
comprension también fueron de enorme ayuda porque con estas son con las que
hacemos crecer la lista de instrucciones e incluso se podría mejorar haciendo
listas de instrucciones válidas dados los estados de los cantaros pero eso es
algo que se me ocurrió mientras escribo esto... bueno, el punto es que podemos
con las listas por comprension podemos aumentando las posibles soluciones de
una manera fácil y rápida.
Nuestro algoritmo es sensible a la salida, es decir, que va a tardar en función de qué tan larga sea la lista de instrucciones que resuelva el problema
planteado.

Para ejecutar el programa hacemos
$ ghci cantaros.hs

Ya en el modo interactivo de haskell con el módulo cargado escribir en la
terminal:

$ cantaros a b r

Dónde a es la capacidad del cantaro A, b es la capacidad del cantaro B y r
es la cantidad de agua que deseamos en alguno de los dos cantaros después de ejecutar una a una las instrucciones de la solución.

Por ejemplo para
$ cantaros 5 3 2

Se responde la siguiente lista de instrucciones:
["Llena el cantaro A en el rio","Vacia el cantaro A en el cantaro B"]

Siguiendo las instrucciones el cantaro A va a terminar conteniendo 2 litros
de agua en su interior.