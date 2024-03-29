-- Напишите функцию partitionN :: [a] -> Int ->[[a]], которая разбивает заданный список на указанное количество подсписков. N-1 подсписков должны иметь одинаковую длину, последний элемент списка списков может содержать меньшее количество элементов. Предусмотреть контроль входных данных.

-- Например partitionN [1,2,3,4,5,6,7] 3 даст результат [[1,2,3],[4,5,6],[7]]

-- partitionN [1,2,3,4,5,6,7] 4 даст результат [[1,2],[3,4],[5,6],[7]]

partitionN :: [a] -> Int ->[[a]]
partitionN l n
  | n == 1 = [l]
  | length(l) > n = [fst (splitAt (length(l) `quot` (n-1)) (drop dropCount l)) | dropCount <- [0,(length(l) `quot` (n-1))..length(l)-1]]
  | length(l) > 0 = [l]
  | otherwise = []