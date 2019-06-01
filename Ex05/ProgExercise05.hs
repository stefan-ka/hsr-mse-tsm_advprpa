module ProgExercises.FS_2019_ProgExer05Prob_V01 where

-- Develop some functions to work with order lists.
-- Make use of higher-order functions and/or recursion.

type ArtName = String   -- name of article
type Number = Int       -- number of ordered articles
type Order = (ArtName, Number)

type Price = Int   -- price of an article in Rappen
type Pricing = (ArtName, Price)
type PricedOrder = (ArtName, Number, Price)

-- Note: Order and Pricing are exactly the SAME type.
-- However, we distinguish them on the software engineering level,
-- but we must be careful.

ol01 :: [Order]
ol01 =
  [("Schraube M4", 100),
   ("Mutter M4", 100),
   ("Unterlegscheibe M4", 200)]

pl01 :: [Pricing]
pl01 =
  [("Schraube M4", 5),
   ("Unterlegscheibe M4", 2),
   ("Mutter M4", 5),
   ("Zahnrad 36Z", 1300)]

-- Given a name and a list of name item pairs, myLookup returns the first
-- item in the list that matches the given name.
-- If the list does not contain the given name, myLookup fails.
-- Later we will write a better function that returns a value indicating
-- whether an item has been found or not.
exa_myLookup =
  myLookup 39 [(5, 'a'), (39, 'b'), (7, 'c'), (39, 'd')] == 'b'

myLookup :: Eq a => a -> [(a, b)] -> b
myLookup num xs = snd (head (filter (\x -> fst x == num) xs))

-- Given an order list and a pricing list, addPrices adds the prices
-- according to the pricing list to the order list.
-- Precondition:
--   All article names in the order list occur in the pricing list.
exa_addPrices =
  addPrices ol01 pl01 ==
    [("Schraube M4",100,500),
     ("Mutter M4",100,500),
     ("Unterlegscheibe M4",200,400)]

addPrices :: [Order] -> [Pricing] -> [PricedOrder]
addPrices os ps = map (\(name, num) -> (name, num, num * myLookup name ps)) os

-- totalPrice determines the total price of an order list.
exa_totalPrice =
  totalPrice (addPrices ol01 pl01) == 1400

totalPrice :: [PricedOrder] -> Price
totalPrice pol = sum (map (\(_, _, price) -> price) pol)
-- Alternative: totalPrice = foldr (\(_, _, p) accu -> accu + p) 0

-- totalNumPrice determines the total number of items and the total price
-- of an order list.
totalNumPrice :: [PricedOrder] -> (Number, Price)
totalNumPrice pol = (totalNum pol, totalPrice pol)
  where totalNum pol = sum(map (\(_, x, _) -> x) pol)

totalNumPrice' :: [PricedOrder] -> (Number, Price)
totalNumPrice' pol = (sum nums, sum prices)
  where (_, nums, prices) = unzip3 pol

-- Returns items that (for the number ordered) cost more than a given maxPrice.
exa_tooExpensive =
  tooExpensive 450 (addPrices ol01 pl01) ==
    [("Schraube M4",100,500),
     ("Mutter M4",100,500)]

tooExpensive :: Price -> [PricedOrder] -> [PricedOrder]
tooExpensive mp pol = filter (\(_, _, price) -> price > mp) pol

-- Adds an order to an order list.
-- If the article name added already occurs in the order list,
-- the number is accordingly incremented.
exa_add =
  addOrder ("Mutter M4", 250) ol01 ==
    [("Schraube M4", 100),
     ("Mutter M4", 350),
     ("Unterlegscheibe M4", 200)]

addOrder :: Order -> [Order] -> [Order]
addOrder (name, num) ((name', num') : ol)
  | name == name' = (name, num + num') : ol
  | otherwise = (name', num') : addOrder (name, num) ol
addOrder nameNum [] = [nameNum]

-- addOrderList adds all orders of a new order list to an old order list.
exa_addOrderList =
  addOrderList (tail ol01) ol01 ==
    [("Schraube M4", 100),
     ("Mutter M4", 200),
     ("Unterlegscheibe M4", 400)]

addOrderList :: [Order] -> [Order] -> [Order]
addOrderList olNew olOld = foldr addOrder olOld olNew

-- Removes an order with a given article name from a given order list.
exa_removeOrder =
  removeOrder "Schraube M4" ol01 ==
    [("Mutter M4", 100),
     ("Unterlegscheibe M4", 200)]

removeOrder :: ArtName -> [Order] -> [Order]
removeOrder toDel xs = filter (\(name, _) -> name /= toDel) xs
-- removeOrder toDel (x@(name, _) : xs)
--   | toDel == name = xs
--   | otherwise     = x : removeOrder toDel xs

