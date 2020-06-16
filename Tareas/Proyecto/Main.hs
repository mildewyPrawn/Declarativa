import System.Environment
import Text.Printf

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
concater = foldr (\x -> (\y -> x ++ " " ++ y)) []

main = do
  args <- getArgs
  case args of
    [file] -> do
      (t, c) <- parseFile file
      printf "YEIH" -- TODO: EJECUTAR EL PROGRAMA
    _ -> printf "Error. No hay argumentos.\n"
      
                          

