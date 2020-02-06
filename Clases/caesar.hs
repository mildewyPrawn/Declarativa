import Data.Char 

-- Función que se encarga de transformar un Char a su indice 
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- Transofrma el indice de un carácter en el Char correspondiente
in2let :: Int -> Char
in2let i = chr (ord 'a' + i)

-- Hace el cifrado por carácter haciendo el corrimiento según el parámetro Entero
shift :: Int -> Char -> Char
shift n c 
 | isLower c = in2let $ (let2int c + n) `mod` 26
 | otherwise = c

-- Hace la codificación de un String, en donde el corrimiento depende del parámetro Entero.
encode :: Int -> String -> String
encode n s = [shift n x | x <- s]

-- Cifrado Caesar
caesar :: String -> String
caesar = encode 3
