
Especificación:

> mss :: [Int] -> Int

> maxl = maximum

> mss = maxl . map sum . segs


maxl devuelve el máximo de una lista de enteros.

maxl [] = error
maxl (n:ns) = max n (maxl ns)

max es máximo de dos enteros.

sum devuelve la suma de los elementos de una lista de enteros.

sum [] = 0
sum (n:ns) = n + sum ns

segs devuelve todos la lista de segmentos de una lista.


> segs = concat . map inits. tails

se calculan los sufijos de la lista, y se sacan los prefijos de cada
sufijo. Esto tiene complejidad n³.


> tails [] = [[]]

> tails (x:xs) = (x:xs): tails xs

> inits [] = [[]]

> inits (x:xs) = [] : map (x:) (inits xs)

concat [] = []

concat (xs:xss) = xs ++ concat xss


-------------------------------------------------------------
Síntesis de un programa lineal

Leyes necesarias:

I. map f . concat = concat . map (map f)

II. maxl . concat = maxl . map max  (si el argumento de concat es una lista no vacia de listas
                                     no vacias, pues maxl [] = undefined)

III.  map (f.g) = map f . map g  2a ley funtorial.

IV. Ley de fusión para foldr: si f es estricta, f a = b  y 
                                 f (g x y) = h x (f y), para toda x,y 
                              entonces
                                 f . foldr g a = foldr h b
				 
   recordemos que una funcion es estricta si f undef = undef


mss = maxl . map sum . segs

    = maxl . map sum . concat . map inits . tails   (por def. de segs)

    = maxl . concat . map (map sum) . map inits . tails   (por ley I)

    = maxl . map maxl . map (map sum) . map inits . tails  (por ley II)

    = maxl . map (maxl . map sum) . map inits . tails (por ley III)

    = maxl . map (maxl . map sum . inits) . tails (por ley III y 
                                                      asociatividad de .)
    
Analizamos ahora la función  

mps = maxl . map sum . inits

que saca los prefijos de una lista (inits), la suma de cada uno (map sum) y toma el máximo (maxl). Es
decir calcula la máxima suma de los prefijos.
  
dado que inits es un fold: inits = foldr g a  donde  a = [[]] y 
                                                     g = \ x xxs -> [] : map (x:) xss) 
 

entonces:

> mps :: [Int] -> Int

> mps = maxl . map sum . foldr (\ x xss -> []: map (x:) xss) [[]]
        
aplicamos la ley IV (fusión) con f = map sum  (hay que checar que f es estricta)


map sum . foldr g a  = fold r h b  donde 

 f (g x xss) = h x (f xss), es decir

map sum ([] : map (x:) xss) = h x (map sum xss)

derivamos la definición de h como sigue:

map sum ([] : map (x:) xss) =
                                  (def. de map)
0: map sum (map (x:) xss) =  
                                  (2a ley funtorial)

0: map (sum . (x:)) xss  =    Pues (sum . (n:)) ns  = sum (n:ns) 
                                                          = n + sum ns 
                                                          = (n+) (sum ns) 
                                                          = ((n+) . sum) ns 

0: map ((x+) .sum) xss)  =
                                  (2a ley funtorial)
0: map (x+) (map sum xss) =
                                   definimos h así: h x xss = 0: map (x+) xss
h x (map sum xss)
 
Luego entonces,

mps = maxl . foldr h [0]   (pues f [] = map sum [] = [0])

> mps2 :: [Int] -> Int

> mps2 = maxl . foldr (\ x xss -> 0:map (x+) xss) [0]

Ahora podemos fusionar nuevamente


maxl . foldr h [0] =  foldr k b con maxl [0] = b = 0 y  
                                    maxl (h x xs) = k x (maxl xs) 

derivamos la def. de k:

maxl (h x xs) = maxl (0:map (x+) xs)   (def. de h)
              = max 0 (maxl (map (x+) xs))  (def. de maxl)
              = max 0 (x + maxl xs)   (el max se distribuye sobre la suma, ojo:
                                          maxl [] = undefined)
luego entonces,

k x n = max 0 (x+n)


De esta manera concluimos que

mps = foldr k 0 = foldr (\ x n -> max 0 (x+n)) 0

> mps3 :: [Int] -> Int

> mps3 = foldr (\x n -> max 0 (x+n)) 0

Desdoblando esta definición para hacer explícita la recursión se tiene que:

> mps4 [] = 0

> mps4 (x:xs) =  max 0 (x + mps xs) 


Seguimos ahora con mss:

mss = maxl . map (maxl . map sum . inits) . tails

    = maxl . map mps . tails

    = maxl . map (foldr k 0) . tails



La función scanr:


La función scanr aplica un operador foldr desde la derecha a todos los sufijos de una lista de entrada, es decir:


scanr :: (a -> b -> b) -> b -> [a] -> [b]

recuerdese que foldr :: (a-> b -> b) -> b -> [a] -> b

entonces scanr puede definirse como:

scanr f b as =  map (foldr f b) . tails

Esta especificación es ejecutable aunque no eficiente y puede optimizarse también mediante un proceso de síntesis ecuacional.

Finalmente, obtenemos el siguiente programa :

> mss2 = maxl . scanr (\ x n -> max 0 (x+n)) 0

