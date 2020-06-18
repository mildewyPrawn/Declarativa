import System.Environment
import Text.Printf
import Proyecto
import Music

-- | f. Función que valida si es o no un comentario.
--      Está aparte por si queremos cambiar la validación
f :: Eq a => a -> [a] -> Bool
f cmt = \x -> x !! 0 /= cmt

-- | parseFile. Función que lee el archivo, y nos separa el tiempo y los acordes.
parseFile :: FilePath -> IO (Float, String)
parseFile file =
  do
    chords <- readFile file
    let (a,b) = (\(x:xs) -> (x,xs)) $ cleanComment (f '#') $ lines chords
    return (read a :: Float, concater b)

-- | cleanComment. Función que quita los comentarios.
cleanComment :: (String -> Bool) -> [String] -> [String]
cleanComment p []     = []
cleanComment p (x:xs) = if p x
                        then [x] ++ cleanComment p xs
                        else cleanComment p xs

-- | concater. Función que concatena las líneas de los acordes del archivo.
-- TODO: en vez de " " concatenar "S" que significa silencio. i.e un silencio
-- cada saltao de línea
concater :: [String] -> String
concater = foldr (\x -> (\y -> x ++ " " ++ y)) []

-- | MAIN.
main = do
  args <- getArgs
  case args of
    [file] -> do
      (t, c) <- parseFile file
      let new_c = translate c
      _ <- play 7 new_c
      return ()
    _ -> printf "Error. No hay argumentos.\n"

-- | parte interactiva del main.
interactive file = do
  (t, c) <- parseFile file
  _ <- play t $ translate c
  return "Ta Tan"
  -- return $ translate c
  -- printf "YEIH\n" -- TODO: EJECUTAR EL PROGRAMA
