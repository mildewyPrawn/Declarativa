module Music where

import Data.Foldable
import System.Process
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Text.Printf

type Hz = Float
type Pulse = Float
type Samples = Float
type Seconds = Float
type Semitones = Float
type Time = Float

volume :: Float
volume = 0.5

outputFilePath :: FilePath
outputFilePath = "output.bin"

sampleRate :: Samples -- frecuency
sampleRate = 300000.0

pitchStandard :: Hz
pitchStandard = 440.0

-- https://en.wikipedia.org/wiki/Piano_key_frequencies
fp :: Semitones -> Hz
fp n = pitchStandard * (2 ** (1.0 / 12.0)) ** (n - 49.0)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ map sin $ map (* step) [0.0 .. sampleRate * duration]
  where
    step = (hz * 2 * pi) / sampleRate

wave :: Time -> [Semitones] -> [Pulse]
wave time l = concat $ map (\x -> freq (fp x) time) l

save :: FilePath -> Time -> [Semitones] -> IO ()
save filePath time l = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE $ wave time l

play :: Time -> [Semitones] -> IO ()
play time l = do
  save outputFilePath time l
  _ <- runCommand $ printf "ffplay -f f32le -ar %f %s" sampleRate outputFilePath
  return ()
