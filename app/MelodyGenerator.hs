module MelodyGenerator
  ( Note
  , randomNote
  , generateMelody
  , noteToMusic
  , melodyToMusic
  ) where

import Euterpea
import System.Random

pitches :: [String]
pitches = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

octaves :: [Int]
octaves = [1, 2, 3, 4, 5]

durations :: [Double]
durations = [ 0.10 ,0.25, 0.5 , 1.0]

type Note = (String, Int, Double)

--генерация одной случайной ноты
randomNote :: Int -> Note
randomNote seed = (notePitch, octave, noteDur) --возращает кортеж (нота, октава, длительность) Например, ("C", 4, 0.5) означает ноту C в 4-й октаве с длительностью 0.5 (половина четвертной ноты).
  where
    gen = mkStdGen seed --генератор случайного чисела
    pitchIdx = fst $ randomR (0, length pitches - 1) gen --индекс ноты (от 1 до 11)
    (octaveIdx, gen') = randomR (0, length octaves - 1) gen --индекс октавы (от 1 до 5)
    (durationIdx, _) = randomR (0, length durations - 1) gen' --индекс длительности (от 1 до 5)
    notePitch = pitches !! pitchIdx --возращает ноту по индексу (индекс был сгенерирован выше)
    octave = octaves !! octaveIdx --возращает октаву по индексу 
    noteDur = durations !! durationIdx --возвращает длительность по индексу 

-- создание списка из n случайных нот (n вводит юзер)
generateMelody :: Int -> Int -> [Note]
generateMelody n seed
  | n <= 0    = [] --если n меньше или равно 0 , то возращет пустой список
  | otherwise = randomNote seed : generateMelody (n - 1) (seed + 1) --если n больше 0, тo вызывается функция randomNote с новым seed для создания одной рандомной ноты
                                -- : добовлят ноту в начало списка
                                --рекурсивно вызывается функция generateMelody с с уменьшенным количеством нот (n-1) и увеличенным seed (seed+1), чтобы создать раные ноты

                                
-- Конвертация нот из кортежа Note в музыкальный объект типа Music Pitch, коробый используеться библиотекой Euterpea для воспроизведения музыки
noteToMusic :: Note -> Music Pitch --входной параметр - кортеж (нота, октава, длительность), выходной параметр - музыкальный объект
noteToMusic (notePitch, octave, noteDur) = --функция принимает кортеж и делит его на 3 компонента
  let pitchClass = case notePitch of --преобразует стринг представление ноты в тип PitchClass
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
        _    -> C -- Если нота не распознана, по умолчанию используется C, то есть До
      --absPitch = 12 * (octave + 1) + pcToInt pitchClass
      durInBeats = noteDur * 4 -- преобразовывает доли четвертной ноты в доли целой ноты (например, 0.5 четвертной ноты = 2 доли целой ноты), так как Euterpea работает с beats (целыми нотами)
  in note (toRational durInBeats) (pitchClass, octave) --создание муз.объекта с помощью функции note. Note принимает 2 аргумента: длительность в виде рационального числа и кортеж (PitchClass, Octave)
--Результат объект Music Pitch, представляющий ноту с заданной высотой, октавой и длительностью.

-- Преобразование списка нот в музыкальный объект Music Pitch, чтобы проиграть мелодию последовательно
melodyToMusic :: [Note] -> Music Pitch
melodyToMusic melody = line $ map noteToMusic melody --функция line принимает список музыкальных объектов и соединяет их в последовательность
-- map noteToMusic melody - преобразует каждую ноту в музыкальный объект с помощью функции noteToMusic
