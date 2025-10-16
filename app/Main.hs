module Main (main) where

import Euterpea
import MelodyGenerator
import Text.Read (readMaybe)
import System.IO
import System.Environment (getArgs)

-- Воспроизведение мелодии
playMelody :: [Note] -> Double -> IO ()
playMelody melody bpm = do
  let music = tempo (toRational bpm / 60) (melodyToMusic melody) --melodyToMusic melody преобразует список нот в объект Music Pitch, как описано выше.
  -- tempo (toRational bpm / 60) применяет модификатор темпа к мелодии
  -- В Euterpea функция tempo :: Rational -> Music a -> Music a - изменяет скорость воспроизведения мелодии.toRational bpm / 60 - задает bpm
  -- Изначало скорость 100 bpm, то есть 100 ударов в минуту, но Euterpea требует скорость в виде отношения к 60 (количество секунд в минуте).
  play music --функция play из Euterpea воспроизводит музыкальный объект

-- отображает каждую ноту из списка melody на экран
printMelody :: [Note] -> IO ()
printMelody melody = mapM_ (putStrLn . show) melody

-- Main function
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- отключает буферизацию,текст выводится в консоль немедленно, без задержек.
  args <- getArgs --получает данные из командной строки. Если запущенно "cabal v1-run Haskell-mel-gen -- generateMelody 5" , то args = ["generateMelody", "5"]
  case args of
    ["generateMelody", nStr] -> case readMaybe nStr :: Maybe Int of 
        -- case args of ["generateMelody", nStr] -> Проверяется, что аргументов ровно два и первый аргумент — "generateMelody"
        -- case readMaybe nStr :: Maybe Int of Just n | n > 0 -> ...  Пытается преобразовать второй аргумент nStr в целое число n.
        -- Если преобразование успешно и n больше 0, выполняется блок кода внутри Just
      Just n | n > 0 -> do
        let melody = generateMelody n 1 -- создаёт список из n нот, начиная с seed = 1
        putStrLn "\nGenerated melody:" --выводит сообщение в консоль
        printMelody melody --выводит каждую ноту из списка melody на экран
        putStrLn "\nPlaying melody..." --выводит сообщение в консоль
        playMelody melody 100 --воспроизводит мелодию с темпом 100 ударов в минуту (bpm)
      _ -> putStrLn "Error: Please provide a valid number of notes (>0)." --если nStr не является числом или n <= 0), выводится это сообщение об ошибке
    _ -> putStrLn "Usage: Haskell-mel-gen generateMelody <number-of-notes>" --Если аргументы не соответствуют этому шаблону с 86 строчки, то выводится сообщение, как правильно запустить программу
