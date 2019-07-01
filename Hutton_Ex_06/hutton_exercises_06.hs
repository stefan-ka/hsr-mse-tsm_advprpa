module ProgExercises.HS_2019_Hutton06 where

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n e = e : replicate' (n - 1) e

-- select nth element of list
(!!!) :: [a] -> Int -> a
(!!!) xs n
  | n == 0 = head xs
  | otherwise = tail xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x : xs) = e == x || elem' e xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take halveLen xs)) (msort (drop halveLen xs))
  where halveLen = quot (length xs) 2

