-- Las secciones son equivalentes al uso de lambdas Por ejemplo: 

-- (x+)	= (+) x = \y -> x+y
-- (+y)	= (flip (+) y) = \x -> x+y
-- (+)	= \x y -> x+y

-- Lista con todas las potencias de 2
-- En esta definición de utiliza la función ^ por segmentos
pwr2 :: [Int]
pwr2 = map ((^)2) [0..]

-- Lista con todos los cuadrados
-- Para está definición se usa de igual forma la función ^por segmentos pero ahora el parámetro que conocemos es el segundo entonces se usa flip
sqr :: [Int]
sqr = map ((flip (^)) 2) [0..]

