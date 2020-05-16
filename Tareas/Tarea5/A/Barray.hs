module Barray where 

class BArray ar where
 
 -- El arreglo vacío
 emptyArray :: ar b 
 -- Test para verificar si un arreglo es vacío
 isEmpty :: ar b -> Bool
 -- regresa el número de elementos de un arreglo
 size :: ar b -> Int
 -- recupera un elemento
 (!) ::  ar b -> Int -> b
 --  modifica el valor de un elemento 
 update :: ar b -> Int -> b -> ar b
 -- agrega un elemento al inicio
 lowExt :: b -> ar b -> ar b
 -- elimina el primer elemento
 lowRem :: ar b -> ar b
 -- agrega un elemento al final
 highExt :: b -> ar b -> ar b
 -- elimina el último elemento
 highRem :: ar b -> ar b
 -- crea un arreglo con copias del mismo elemento
 copy :: b -> Int -> ar b
 -- la función map para la estructura
 mapArr :: (b -> c) -> ar b -> ar c
 -- construye un arreglo a partir de una lista
 fromList :: [b] -> ar b
 -- convierte el arreglo en una lista
 toList :: ar b -> [b]
