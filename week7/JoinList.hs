module JoinList where

import Data.Monoid
import Scrabble
import Sized


-- Exercise 1

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving Show

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2


-- Exercise 2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i _ | i < 0    = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i jl@(Append m jl1 jl2)
    | i > jlSize jl  = Nothing
    | i < jlSize jl1 = indexJ i jl1
    | otherwise      = indexJ (i - jlSize jl1) jl2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                 = Empty
dropJ 0 jl                    = jl
dropJ n (Single _ a)          = Empty
dropJ n jl@(Append m jl1 jl2)
    | n >= jlSize jl = Empty
    | n > jlSize jl1 = dropJ (n - jlSize jl1) jl2
    | otherwise      = (dropJ n jl1) +++ jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ 0 _               = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m jl1 jl2)
    | n >= jlSize jl  = jl
    | n <= jlSize jl1 = takeJ n jl1
    | otherwise       = jl1 +++ takeJ (n - jlSize jl1) jl2

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
