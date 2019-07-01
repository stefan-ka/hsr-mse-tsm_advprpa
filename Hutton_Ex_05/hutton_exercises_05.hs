module ProgExercises.HS_2019_Hutton05 where

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [xs !! i * ys !! i | i <- [0..length xs - 1]]

