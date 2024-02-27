-- 2. Скопировать информацию из одного файла в другой, заменив знаки  пунктуации заданным с клавиатуры  символом. Имена файлов указываются в командной строке. #5

import System.IO
import System.Environment
import Data.Char

changePunctuationChars :: Char -> String -> String
changePunctuationChars ch st = map (\c -> if (isPunctuation c) then ch else c) st

main :: IO ()
main = do
    [inputFileName, outputFileName] <- getArgs
    ch <- getChar
    contents <- readFile inputFileName

    writeFile outputFileName (changePunctuationChars ch contents)
