module Numbers where

 

units :: [String]

units = ["cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve"]

teens :: [String]
teens = ["diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]

tens :: [String]
tens = ["veinte","treinta","cuarenta","cincuenta","sesenta","setenta", "ochenta","noventa"]

hun :: [String]
hun = ["cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]

convert1 :: Int -> String 
convert1 i = units!!i

digits :: Int -> Int -> (Int,Int)
digits m n = (n `div` m, n `mod` m)

combine2 :: (Int,Int) -> String
combine2 (d,u)
 | d == 0 = convert1 u
 | d == 1 = teens!!u
 | u == 0 = tens!!(d-2)
 | d == 2 = "veinti" ++ convert1 u
 | otherwise = tens!!(d-2) ++ " y " ++ convert1 u

convert2 :: Int -> String
convert2 = combine2 . digits 10

combine3 :: (Int,Int) -> String
combine3 (c,r)
 | r == 0 = hun!!(c-1)
 | c == 1 = "ciento " ++ convert2 r
 | c == 5 = "quinientos " ++ convert2 r
 | c == 7 = "setecintos " ++ convert2 r
 | c == 9 = "novecientos " ++ convert2 r
 | otherwise = (convert1 c) ++ "cientos " ++ convert2 r

convert3 :: Int -> String
convert3 = combine3 . digits 100

combineR :: (Int,Int) -> String
combineR (m,r)
 | m == 1 && r == 0 = "mil"
 | r == 0 = (convert m) ++ " mil"
 | m == 1 = "mil " ++ convert3 r
 | otherwise = (convert m) ++ " mil " ++ (convert3 r)

convert :: Int -> String
convert n 
 | n < 10 = convert1 n
 | n < 100 = convert2 n
 | n < 1000 = convert3 n
 | n < 1000000 = combineR $ digits 1000 n
