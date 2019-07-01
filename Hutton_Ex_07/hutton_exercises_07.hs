module ProgExercises.HS_2019_Hutton07 where

-- comprehension [f x | x <- xs, p x] with map and filter
mapf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapf f p xs = map f (filter p xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e accu -> f e : accu) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\e accu -> if p e == True then e : accu else accu) []

