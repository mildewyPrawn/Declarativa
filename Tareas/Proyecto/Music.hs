module Music where

import Data.Foldable
import System.Process
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Text.Printf

type Hz = Float -- 
type Pulse = Float -- 
type Samples = Float -- Cantidad de ondas en un segundo, entre más grande, más grave
type Seconds = Float -- Segundos (tiempo)
type Semitones = Float -- Semitono
type Time = Float -- Tiempo de la onda

-- | volume. Constante para el volumen.
volume :: Float
volume = 0.5

-- | sampleRate. Cantidad de ondas en un segundo para reproducirlas.
sampleRate :: Samples
sampleRate = 300000.0

-- | pitchStandard. Constante del estandar para la nota La4.
pitchStandard :: Hz
pitchStandard = 440.0

-- https://en.wikipedia.org/wiki/Piano_key_frequencies
-- | fp. Función para calcular los hz que produce una nota dada. La referencia
--       está dada por la tecla 49 de un piano.
fp :: Semitones -> Hz
fp n = pitchStandard * (2 ** (1.0 / 12.0)) ** (n - 49.0)

-- | freq. Función que  dados los hz y los segundos, nos regresa una lista con
--         los valores acotados para producir un segundo de sonido.
freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ map sin $ map (* step) [0.0 .. sampleRate * duration]
  where
    step = (hz * 2 * pi) / sampleRate

-- | wave. Función que genera las ondas que van a producir el sonido.
wave :: Time -> [Semitones] -> [Pulse]
wave time l = concat $ map (\x -> freq (fp x) time) l

-- | save. Función que guarda el archivo con las ondas.
save :: FilePath -> Time -> [Semitones] -> IO ()
save filePath time l = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE $ wave time l

-- | play. Función que ejecuta ffplay con ciertas banderas para reproducir la
--         salida
play :: Time -> [Semitones] -> IO ()
play time l = do
  save "cancion.bin" time l
  _ <- runCommand $ printf "ffplay -f f32le -ar %f cancion.bin" sampleRate
  return ()
