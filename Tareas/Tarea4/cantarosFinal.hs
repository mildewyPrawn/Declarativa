{-
- Programacion Declarativa 2020-1
- Tarea 4: I want to play a game
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Cantaros where

 type Action = String

 {-Funcion principal, recibe tres enteros, el primero sera la capacidad
 del cantaro A, la segunda la capacidad del cantaro B y la tercera la
 cantidad de agua que se desea que quede en alguno de los dos cantaros
 Regresa una lsita de las instrucciones para resolver el problema.-}
 cantaros :: Integer-> Integer-> Integer-> [Action]
 cantaros c1 c2 f
            | c1<f && c2<f = ["imposibol"]
            | c1 == f = ["Llena el cantaro A en el rio"]
            | c1 == f = ["Llena el cantaro B en el rio"]
            | otherwise = map acciones (resuelve1 c1 c2 f l1)
            where l1 = [(c1,0,[1]),
                        (0,c2,[2]),
                        (0,0,[3]),
                        (0,0,[4]),
                        (0,0,[5]),
                        (0,0,[6])]

 --Por simplicidad tratamos los eventos como un número, al final solo mapeamos
 --la lista de numeros con esta funcion para obtener una lista de acciones
 acciones :: Integer-> Action
 acciones n
        |n == 1 = "Llena el cantaro A en el rio"
        |n == 2 = "Llena el canatro B en el rio"
        |n == 3 =  "Vacia el cantaro A en el cantaro B"
        |n == 4 =  "Vacia el cantaro B en el cantaro A"
        |n == 5 =  "Vacia el cantaro A en el rio"
        |n == 6 =  "Vacia el cantaro B en el rio"

 {- resuelve recibe la capacidad de el cantaro A c1, la capacidad del
 cantaro B c2, la cantidad final que buscamos f y una lista de ternas (
 a1,a2,l), en las que a1 y a2 es la cantidad de agua actual que contienen
 los cantaros A y B respectivamente despues de ejecutar todos los pasos de
 la lista de "acciones" de l. Si en estas ternas hay alguna l que resuelve
 el ejercicio regresa dicha l, en caso contrario aumenta todos los pasos
 validos posibles a l y vuelve a revisar si ya se resolvio -}
 resuelve1 :: Integer -> Integer -> Integer -> [(Integer, Integer, [Integer])] -> [Integer]
 resuelve1 c1 c2 f l =  if null r
                        then resuelve1 c1 c2 f (aumenta c1 c2 f l) else r
                        where r = check1 f l


 --check1 revisa que en la lista de tripletas (a1,a2,l) exista una solucion,
 --si no existe solucion entonces regresa la lista vacía, en caso contrario
 --regresa la lista de "acciones" que es solucion
 check1 :: Integer -> [(Integer, Integer, [Integer])] -> [Integer]
 check1 f [] = []
 check1 f ((a1,a2,l):xs) = if f == a1 || f== a2
                                then l
                                else check1 f xs


 --aumenta recibe la capacidad del cantaro A c1, la capacidad del cantaro B c2
 --la cantidad fnal deseada f y una lista de tripletas (a1,a2,l) que guardan
 --el estado de los cantaros despues de aplicar la serie de acciones a y
 --a cada tripleta de la lista l le aumenta todos los posibles pasos
 --validos en uno y actualiza a1 y a2 despues de aplciar la accion agregada.
 aumenta :: Integer -> Integer -> Integer -> [(Integer, Integer, [Integer])]
            -> [(Integer, Integer, [Integer])]
 aumenta c1 c2 f l = eval c1 c2 f (concat $ map aumenta1 l)

 --Esto salia con un map (\x -> test1 c1 c2 f) (concat $ map aumenta1 l)
 --pero no me daban los tipos... solo es hacer la actualizacion de a1 y a2
 --a cada tripleta
 eval :: Integer -> Integer -> Integer -> [(Integer, Integer, [Integer])]
         -> [(Integer, Integer, [Integer])]
 eval _ _ _ [] = []
 eval c1 c2 f (x:xs) = (test1 c1 c2 f x)++ (eval c1 c2 f xs)

 --Le aumenta a la lista l todas las posiblesa accionesa válidas.
 aumenta1 :: (Integer, Integer,[Integer]) -> [(Integer, Integer,[Integer])]
 aumenta1 (a1,a2,l) = [(a1,a2, l ++ [y]) | y<-[1,2,3,4,5,6], y /= last l]

 --Regresa la tripleta con a1 y a2 actualizados dado el último paso agregado
 --de la lista de acciones l
 test1 :: Integer -> Integer -> Integer -> (Integer, Integer, [Integer]) ->
          [(Integer, Integer, [Integer])]
 test1 c1 c2 f (a1,a2,l)
                |lst==1 = [(c1,a2,l)]
                |lst==2 = [(a1,c2,l)]
                |lst==3 = [((catch0 (a1-(c2-a2))), a2f, l)]
                |lst==4 = [(a1f,(catch0 (a2-(c1-a1))),l)]
                |lst==5 = [(0,a2,l)]
                |lst==6 = [(a1,0,l)]
                where (lst,a1f,a2f) = (last l,trunca c1 a1 a2, trunca c2 a2 a1)

 --Funcion para atrapar los casos en los que los que puede quedar una cantidad
 --negativa en alguno de los cantaros.
 catch0 :: Integer-> Integer
 catch0 n = if n < 0 then 0 else n

 --trunca la suma de aR y aD a la capacidad del cantaro.
 trunca :: Integer-> Integer-> Integer-> Integer
 trunca c aR aD = if aR + aD > c then c else aR + aD
