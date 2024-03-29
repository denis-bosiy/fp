-- Напишите функцию, строящую список подсписков чисел: в первом подсписке будут степени единицы, во втором степени двойки, в третьем - тройки и так далее:

-- [[1,1,1,...],[2,4,8,...],[3,9,27 ,...],...]

-- Количество элементов в подсписках, а также максимальное число, участвующее в образовании подсписков степеней числа, являются параметрами функции.

-- Для предложенного примера [[1,1],[2,4],[3,9],[4,16]] количество элементов в подсписках= 2, мак число=4.

makeListOfSublists :: Int -> Int -> [[Int]]
makeListOfSublists n m
  | m > 0 = makeListOfSublists n (m-1) ++ [[m ^ i | i <- [1..n]]]
  | otherwise = []