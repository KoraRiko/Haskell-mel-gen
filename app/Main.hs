module Main where

import Euterpea
import System.Random
import Text.Read (readMaybe)
import System.IO
import System.Environment (getArgs)

-- Constants
pitches :: [String]
pitches = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

octaves :: [Int]
octaves = [3, 4, 5]

durations :: [Double]
durations = [0.25, 0.5, 1.0 ]

-- Type for a note: (Pitch, Octave, Duration)
type Note = (String, Int, Double)

-- Random note generation
randomNote :: Int -> Note
randomNote seed = (notePitch, octave, noteDur)
  where
    gen = mkStdGen seed
    pitchIdx = fst $ randomR (0, length pitches - 1) gen
    (octaveIdx, gen') = randomR (0, length octaves - 1) gen
    (durationIdx, _) = randomR (0, length durations - 1) gen'
    notePitch = pitches !! pitchIdx
    octave = octaves !! octaveIdx
    noteDur = durations !! durationIdx

-- Generate a melody of n notes
generateMelody :: Int -> Int -> [Note]
generateMelody n seed
  | n <= 0    = []
  | otherwise = randomNote seed : generateMelody (n - 1) (seed + 1)

-- Convert a note to Euterpea's Music Pitch
noteToMusic :: Note -> Music Pitch
noteToMusic (notePitch, octave, noteDur) =
  let pitchClass = case notePitch of
        "C"  -> C
        "C#" -> Cs
        "D"  -> D
        "D#" -> Ds
        "E"  -> E
        "F"  -> F
        "F#" -> Fs
        "G"  -> G
        "G#" -> Gs
        "A"  -> A
        "A#" -> As
        "B"  -> B
        _    -> C -- Fallback
      --absPitch = 12 * (octave + 1) + pcToInt pitchClass
      durInBeats = noteDur * 4 -- Convert to quarter notes
  in note (toRational durInBeats) (pitchClass, octave)

-- Convert melody to Euterpea Music
melodyToMusic :: [Note] -> Music Pitch
melodyToMusic melody = line $ map noteToMusic melody

-- Play a melody
playMelody :: [Note] -> Double -> IO ()
playMelody melody bpm = do
  let music = tempo (toRational bpm / 60) (melodyToMusic melody)
  play music

-- Print a melody
printMelody :: [Note] -> IO ()
printMelody melody = mapM_ (putStrLn . show) melody

-- Main function
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8 -- Устанавливаем кодировку UTF-8
  args <- getArgs
  case args of
    ["generateMelody", nStr] -> case readMaybe nStr :: Maybe Int of
      Just n | n > 0 -> do
        let melody = generateMelody n 1
        putStrLn "\nGenerated melody:"
        printMelody melody
        putStrLn "\nPlaying melody..."
        playMelody melody 100
      _ -> putStrLn "Error: Please provide a valid number of notes (>0)."
    _ -> putStrLn "Usage: Haskell-mel-gen generateMelody <number-of-notes>"