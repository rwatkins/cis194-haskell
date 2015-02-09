-- http://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf
--
-- Exercise 1: Wholemeal programming
--
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1' and fun2' respectively.
--
-- Hint: For this problem you may wish to use the functions iterate and
-- takeWhile. Look them up in the Prelude documentation to see what they
-- do.

module HW4 where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- My attempt
fun1' :: [Integer] -> Integer
fun1' = (foldl' (\x y -> (*x) . (subtract 2) $ y) 1) . (filter even)


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- My attempt
fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n    = (+n) . fun2' . (`div` 2) $ n
        | otherwise = fun2' . (+1) . (*3) $ n


-- Exercise 2: Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node i _ _ _) = i

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf                   = Node 0 Leaf x Leaf
insertTree x (Node _ lTree y rTree) =
    if height lTree <= height rTree
    then let newLTree = insertTree x lTree
             newHeight = 1 + height newLTree
         in (Node newHeight newLTree y rTree)
    else let newRTree = insertTree x rTree
             newHeight = 1 + height newRTree
         in (Node newHeight lTree y newRTree)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf


-- Exercise 3: More folds!

xor :: [Bool] -> Bool
xor = (foldl' (\x _ -> not x) False) . (filter id)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x coll -> [f x] ++ coll) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exercise 4: Finding primes

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x+1) (filter (not (flip elem) ) . prods
    where prods n = cartProd [1..n] [1..n]
          exclude n = [i+j+2*i*j | (i, j) <- prods n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
