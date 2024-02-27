module LW4 (
  mySingleton,
  myRepeat,
  myMember,
  myIsUpperCase,
  myIntersection
) where

-- Генерирует список из одного элемента
mySingleton :: a -> [a]
mySingleton a = [a]
-- mySingleton 'a' => ['a']
-- mySingleton 1 => [1]
-- mySingleton False => [False]

-- Возвращает список, в котором n раз повторяется число n
myRepeat :: Int -> [Int]
myRepeat a
  | a > 0 = [a | i <- [1..a]]
  | otherwise = []
-- myRepeat -1 => []
-- myRepeat 0 => []
-- myRepeat 4 => [4,4,4,4]

-- Проверяет наличие элемента в множестве
myMember :: Ord a => a -> [a] -> Bool
myMember _ [] = False
myMember x (y:ys)
  | x == y = True
  | otherwise = myMember x ys
-- myMember 1 [] => False
-- myMember 1 [1] => True
-- myMember 3 [1,2,4,5] => False

-- Проверяет, находится ли символ в верхнем регистре
myIsUpperCase :: Char -> Bool
myIsUpperCase c
  | c `elem` ['A'..'Z'] = True
  | c `elem` ['А'..'Я'] = True
  | c == 'Ё' = True
  | otherwise = False
-- myIsUpperCase 'a' => False
-- myIsUpperCase 'D' => True
-- myIsUpperCase 'П' => True
  
-- Возвращает данные из первого отображения для ключей, присутствующих в обоих отображениях
myIntersection :: Ord k => Map.Map k a -> Map.Map k b -> Map.Map k a
myIntersection m1 m2 = Map.filterWithKey (\k _ -> k `elem` (Map.keys (m2))) m1
-- myIntersection (Map.fromList [(1, "one"), (2, "two"), (3, "three")]) (Map.fromList [(2, "dos"), (3, "tres"), (4, "cuatro")]) => Map.fromList [(2,"two"),(3,"three")]
-- myIntersection (Map.fromList [(1, "one"), (2, "two"), (3, "three")]) (Map.fromList [(4, "cuatro")]) => Map.fromList []
-- myIntersection (Map.fromList [(1, "one")]) (Map.fromList []) => Map.fromList []