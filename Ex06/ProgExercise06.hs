import Prelude hiding (flip, curry, uncurry)

-- Exercise 5 (Recursion over Lists)

--delDups deletes duplicates from a list
testDelDups =
  delDups [1,2,3,4,5] == [1,2,3,4,5] &&
  delDups [1,1,1,1,1] == [1] &&
  (delDups [1,2,2,4,1] == [1,2,4] || delDups [1,2,2,4,1] == [2,4,1]) &&
  delDups []          == ([] :: [Int])

delDups :: Eq a => [a] -> [a]
delDups [] = []
delDups (x : xs)
  | elem x xs = delDups xs
  | otherwise = x : (delDups xs)

-- removeEachSnd removes each second element from a list.
testRemoveEachSnd =
  removeEachSnd [1,2,3,4,5,6,7,8] == [1,3,5,7] &&
  removeEachSnd [1,2,3,4,5,6,7]   == [1,3,5,7] &&
  removeEachSnd [1]               == [1] &&
  removeEachSnd []                == ([] :: [Int])

removeEachSnd :: [a] -> [a]
removeEachSnd [] = []
removeEachSnd [x] = [x]
removeEachSnd (x : y : xs) = x : (removeEachSnd xs)

-- makePairs pairs adjacent elements of a list
testMakePairs =
  makePairs [1,2,3,4,5,6,7,8] == [(1,2),(3,4),(5,6),(7,8)] &&
  makePairs [1,2,3,4,5,6,7]   == [(1,2),(3,4),(5,6)] &&
  makePairs [1,2]             == [(1,2)] &&
  makePairs [1]               == [] &&
  makePairs []                == ([] :: [(Int,Int)])

makePairs :: [a] -> [(a, a)]
makePairs (x : y : xs) = (x, y) : (makePairs xs)
makePairs xs = []

testMakePairsV2 =
  makePairsV2 [1,2,3,4,5,6,7,8] == [(1,2),(3,4),(5,6),(7,8)] &&
  makePairsV2 [1,2,3,4,5,6,7]   == [(1,2),(3,4),(5,6)] &&
  makePairsV2 [1,2]             == [(1,2)] &&
  makePairsV2 [1]               == [] &&
  makePairsV2 []                == ([] :: [(Int,Int)])

makePairsV2 :: [a] -> [(a, a)]
makePairsV2 xs = removeEachSnd (zip xs (tail xs))

-- halve divides a list into two lists containing each second element,
-- the first list beginning with the first,
-- the second list beginning with the second
testHalve =
  halve [1,2,3,4,5,6] == ([1,3,5], [2,4,6]) &&
  halve [1,2,3,4,5]   == ([1,3,5], [2,4]) &&
  halve [1]           == ([1], []) &&
  halve []            == ([], [] :: [Int])

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (removeEachSnd xs, removeEachSnd (tail xs))

-- divideList divides a list into chunks of length n each, except
-- of the last chunk, which might be shorter
-- Precondition:
--   n > 0
-- Theorem:
--   For all n > 0 and all xs: concat (divideList n xs) == xs
testDivideList =
  divideList 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]] &&
  divideList 3 [1 .. 9] == [[1,2,3],[4,5,6],[7,8,9]] &&
  divideList 3 [1] == [[1]] &&
  divideList 3 [] == ([] :: [[Int]])

divideList :: Int -> [a] -> [[a]]
divideList _ [] = []
divideList n xs = (take n xs) : divideList n (drop n xs)

-- Exercise 6 (Higher-Order Functions)

-- flip f takes its (first) two arguments in the reverse order of f
testFlip = flip take [1,2,3,4,5] 3 == [1,2,3]

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\a b -> f b a)

-- curry converts a function on pairs to a curried function
testCurry = curry (\(x,y) -> x + y) 3 4 == 7

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b)

-- uncurry converts a curried function to a function on pairs
testUncurry = uncurry (\x y -> x + y) (3, 4) == 7

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b

-- implement reverse using foldr and (++)
reverseR :: [a] -> [a]
reverseR = foldr (\x accu -> accu ++ [x]) []

-- implement reverse using foldl, (:), and flip
reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) []

-- revAppend prepends the first list in reverse order before the second list
testRevAppend = revAppend [3,2,1] [4,5,6] == [1,2,3,4,5,6]
-- implement revAppend using foldl and flip

revAppend :: [a] -> [a] -> [a]
revAppend = flip (foldl (flip (:)))


