-- Напишите функцию elemIndices :: Eq a => a -> [a] -> [Int], которая находит, под какими индексами в списке встречается заданный элемент.

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices el l
  | el `elem` l = filter (\x -> x /= length l) [ if (l !! (i) == el) then i else length l | i <- [0..(length l) -1] ]
  | otherwise = []