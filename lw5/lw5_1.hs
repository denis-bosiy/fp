-- 1. Вывести на экран сформированный список, данные для которого вводятся с клавиатуры: начальное значение, количество элементов, кратность.

-- Например- [14,21,28] 3 элемента списка, начинающиеся с 14, кратные 7. #2

import System.IO

main = do
    hSetBuffering stdout NoBuffering

    start <- readNum
    n <- readNum
    multiplicity <- readNum

    print ([start + (i * multiplicity) | i <- [0..n-1]])

    where
      readNum :: IO Int 
      readNum = readLn