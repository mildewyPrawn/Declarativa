module Snoc where
 import Barray

 data SList a = SNil | Snoc (SList a) a deriving(Eq, Show)
 
 instance - SList where
  -- El arreglo vacío
  emptyArray = SNil

  -- Test para verificar si un arreglo es vacío
  isEmpty SNil = True
  isEmpty _ = False

  -- regresa el número de elementos de un arreglo
  size SNil = 0
  size (Snoc xs _) = 1 + size xs

  -- recupera un elemento
  (Snoc xs x) ! n 
   | n == s    = x
   | n < s     = xs ! n  
   | otherwise = error "index too large"
   where s =  size xs
  SNil ! _ = error "index too large"

  --  modifica el valor de un elemento 
  update l@(Snoc xs x) n e
   | n == s    = Snoc xs e
   | n < s     = Snoc (update xs n e) x  
   | otherwise = l
   where s =  size xs
  update SNil _ _ = SNil

  -- agrega un elemento al inicio
  lowExt e SNil = Snoc SNil e
  lowExt e (Snoc xs x) = Snoc (lowExt e xs) x

  -- elimina el primer elemento
  lowRem (Snoc SNil x) = SNil
  lowRem (Snoc xs x)   = Snoc (lowRem xs) x

  -- agrega un elemento al final
  highExt x xs = Snoc xs x

  -- elimina el último elemento
  highRem (Snoc xs x) = xs

  -- crea un arreglo con copias del mismo elemento
  copy _ 0 = SNil 
  copy e i = Snoc (copy e (i-1)) e

  -- la función map para la estructura
  mapArr _ SNil = SNil
  mapArr f (Snoc xs x) = Snoc (mapArr f xs) (f x)

  -- construye un arreglo a partir de una lista
  fromList [] = SNil
  fromList xs = Snoc (fromList (init xs)) (last xs)

  -- convierte el arreglo en una lista
  toList SNil = []
  toList (Snoc xs x) = (toList xs) ++ [x]
