-- Напишите функцию, которая читает входной текстовый файл и выводит в выходной файл пары (слово:число), где слово - есть каждое уникальное слово файла, а число - количество вхождений этого слова. Пары должны быть отсортированы по убыванию чисел.

import Data.List (sortBy, nub)
import Data.Function (on)
import System.IO.Unsafe
import System.Environment

sortTuplesBySnd :: [(String, Int)] -> [(String, Int)]
sortTuplesBySnd = sortBy (flip compare `on` snd)

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

getWordCount :: String -> String -> Int
getWordCount str text = sum [if (str == (substring i (length (str)+i) text)) then 1 else 0 | i <- [0..(length (text)) - (length (str))]]

makeDictionary :: String -> [(String, Int)]
makeDictionary str = sortTuplesBySnd (zip wordsList [getWordCount (wordsList !! (i)) str | i <- [0..(length(wordsList) -1)]])
  where wordsList = nub (words str)
-- nub = analogue to uniq

tuplesListToString :: [(String, Int)] -> String
tuplesListToString [] = ""
tuplesListToString (x:[]) = show(fst(x)) ++ " " ++ show(snd(x))
tuplesListToString (x:xs) = show(fst(x)) ++ " " ++ show(snd(x)) ++ ['\n'] ++ tuplesListToString xs

printWordsCountsToFile :: String -> String -> IO()
printWordsCountsToFile inputFile outputFile = writeFile outputFile (tuplesListToString (makeDictionary (unsafePerformIO (readFile inputFile))))

main = do
  args <- getArgs
  printWordsCountsToFile (args !! (0)) (args !! (1))
