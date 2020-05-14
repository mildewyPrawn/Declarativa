module List where
 import Barray

 instance BArray [] where

  emptyArray = []

  -- Test para verificar si un arreglo es vacío
  isEmpty = null

  -- regresa el número de elementos de un arreglo
  size = length 

  -- recupera un elemento
  (!) = (!!) 

  --  modifica el valor de un elemento 
  update [] _ _     = error "index too big"
  update (x:xs) 0 e = e:xs
  update (x:xs) n e = x : (update xs (n-1) e)

  -- agrega un elemento al inicio
  lowExt = (:)

  -- elimina el primer elemento
  lowRem = tail

  -- agrega un elemento al final
  highExt x xs = xs ++ [x]

  -- elimina el último elemento
  highRem = init

  -- crea un arreglo con copias del mismo elemento
  copy e i = take i $ repeat e

  -- la función map para la estructura
  mapArr = map

  -- construye un arreglo a partir de una lista
  fromList = id

  -- convierte el arreglo en una lista
  toList = id