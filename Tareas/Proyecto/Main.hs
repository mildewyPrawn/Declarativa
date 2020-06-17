import System.Environment
import Text.Printf
import Proyecto

f :: Eq a => a -> [a] -> Bool
f cmt = \x -> x !! 0 /= cmt

parseFile :: FilePath -> IO (String, String)
parseFile file =
  do
    chords <- readFile file
    let (a,b) = (\(x:xs) -> (x,xs)) $ cleanComment (f '#') $ lines chords
    return (a, concater b)

cleanComment :: (String -> Bool) -> [String] -> [String]
cleanComment p []     = []
cleanComment p (x:xs) = if p x
                        then [x] ++ cleanComment p xs
                        else cleanComment p xs

concater :: [String] -> String
-- TODO: en vez de " " concatenar "S" que significa silencio. i.e un silencio
-- cada saltao de línea
concater = foldr (\x -> (\y -> x ++ " " ++ y)) []

main = do
  args <- getArgs
  case args of
    [file] -> do
      (t, c) <- parseFile file
      let new_c = translate c
      print new_c
      printf "\n" -- TODO: EJECUTAR EL PROGRAMA
    _ -> printf "Error. No hay argumentos.\n"

interactive file = do
  (t, c) <- parseFile file
  return $ translate c
  -- printf "YEIH\n" -- TODO: EJECUTAR EL PROGRAMA
