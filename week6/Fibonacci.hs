{-# Language FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
-- A neat solution someone else came up with:
-- https://github.com/prakashk/cis-194/blob/d19028f9a8b934e5905d64612bf09a332e4661ff/wk06/Fibonacci.hs#L19


-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
    show s = show $ take 20 $ streamToList s


-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)


-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) = (Cons x (Cons y $ interleaveStreams s1 s2))

largestPowerOf2 :: Integer -> Integer
largestPowerOf2 n
    | odd n     = 0
    | otherwise = 1 + (largestPowerOf2 $ div n 2)

ruler :: Stream Integer
ruler = streamMap largestPowerOf2 $ streamFromSeed (+1) 1


-- Exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate (Cons n s) = Cons (n * (-1)) $ negate s
    (+) (Cons n1 s1) (Cons n2 s2) = Cons (n1+n2) (s1+s2)
    (*) (Cons n1 s1) (Cons n2 s2) = Cons (n1*n2) $ (streamMap (*n1) s2) + (streamMap (*n2) s1)

instance Fractional (Stream Integer) where
    -- A/B = Q
    -- Q = (a0/b0) + x((1/b0)(A' - QB'))
    (/) s1@(Cons x a) s2@(Cons y b) = Cons (div x y) $ streamMap (`div` y) (a - (s1/s2) * b)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)


-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix w x y z) = Matrix (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let (Matrix x _ _ _) = (Matrix 1 1 1 0)^n
         in x
