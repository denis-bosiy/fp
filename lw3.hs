-- Описание функций
-- Возвращает список всех чисел от n до 1
listnums :: Int -> [Int]
-- Возвращает последние элементы каждого переданного списка
secondlastlist :: [[Int]] -> [Int]
-- Возвращает объединение двух списков
myunion :: [Int] -> [Int] -> [Int]
-- Возвращает разность двух списков
mysubst :: [Int] -> [Int] -> [Int]
-- Возвращает список из n-х элементов подсписков
getNths :: [[Int]] -> Int -> [Int]

-- Реализация функций

listnums n
  | n == 1 = [1]
  | n > 1 = [n] ++ listnums (n-1)
  | otherwise = []

lastlist l 
  | length l > 0 = drop (length (l) - 1) (l)
  | otherwise = []
secondlastlist l
  | (length l > 0) = lastlist (l !! (0)) ++ secondlastlist (tail l)
  | otherwise = []

removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (\n -> n /= x) xs)
myunion l1 l2 = removeDuplicates (l1 ++ l2)

mysubst l1 [] = uniq l1
mysubst l1 l2 = mysubst (filter (\x -> head(l2) /= x) l1) (tail(l2))
  
getNths l n 
  | n >= 0 = map (\x -> x !! n) (filter (\x -> length (x) > n) l)
  | otherwise = []