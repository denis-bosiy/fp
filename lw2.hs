-- Описание функций
do_my_list :: Int -> [Int]
-- Генерирует список из N элементов, начиная с указанного элемента N
oddEven :: [Int] -> [Int]
-- Переставляет местами соседние элементы входного списка
insert :: [Int] -> Int -> Int -> [Int]
-- Включает в переданный список элемент на заданную позицию
listSum :: [Int] -> [Int] -> [Int]
-- Поочерёдно складывает элементы двух списков. На выходе функции список из двух элементов(суммы первого и второго списков)
position :: [Int] -> Int -> Int
-- Возвращает индекс первого вхождения элемента в список. В случае отсутствия элемента в списке возвращается -1
getSum5 :: Int -> Int
-- Считает сумму от 1 до переданного числа
getSum6 :: Int -> Int
-- Считает сумму по формуле ∑i=1n(n−i)
  
-- Реализация функций
do_my_list n = [ x | x <- [n .. n + n - 1 ] ]

swap l i =  l !! (if (odd i) then (i-1) else (i+1))
shouldSwap l i = (i < length l) && ((i /= (length l - 1) || (odd i)))
oddEven l = [ if (shouldSwap l i) then swap l i else l !! (i) | i <- [0..(length l) - 1] ] 

insert l a n = take n l ++ [a] ++ drop n l

getSum [] = 0
getSum (x:xs) = x + getSum xs
safeIndex l i = if (i < length l) then l !! (i) else 0
listSum l1 l2 = [safeIndex l1 i + safeIndex l2 i | i <- [0..(if length l1 > length l2 then length l1 else length l2) - 1] ]

position l a = if (a `elem` l)
  then minimum [ if (l !! (i) == a) then i else length l | i <- [0..(length l) -1] ]
  else -1

getSum5 1 = 1
getSum5 n = n + getSum5 (n-1)

getSum6' n (-1) = 0
getSum6' n 0 = n
getSum6' n 1 = n - 1
getSum6' n i = n - i + getSum6' n (i-1)

getSum6 n = if (n >= 0) then getSum6' n (n-1) else -1