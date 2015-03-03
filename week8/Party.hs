module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee


-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL lst i) = GL (e:lst) (empFun e + i)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL lstA fA) (GL lstB fB) = GL (lstA ++ lstB) (fA + fB)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


-- Exercise 2

nodeList :: Tree a -> [a]
nodeList (Node {rootLabel=x, subForest=xs}) = x : concat (map nodeList xs)

-- My implementation (not even sure if this is correct)
--treeFold :: (b -> a -> b) -> b -> Tree a -> b
--treeFold f initial tree = foldl f initial $ nodeList tree

-- Copied from https://github.com/elbeno/cis194/blob/bf25eea27409c8d1599d42b9efd36d695729b1d6/homework8/Party.hs#L28
treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f l (Node a ts) = f a $ map (treeFold f l) ts


-- Exercise 3

--combineGLs :: Employee -> [GuestList] -> GuestList

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (bestWith, bestWithout)
    where bestWith    = glCons boss $ mconcat $ map snd gls
          bestWithout = mconcat $ map (uncurry moreFun) gls


-- Exercise 4

-- My implementation (incomplete/broken)
--maxFun :: Tree Employee -> GuestList
--maxFun tree@(Node {rootLabel=e, subForest=xs}) =
--    treeFold (\guestlist employee -> glCons employee guestlist) (GL [e] (empFun e)) tree

-- Copied from https://github.com/elbeno/cis194/blob/bf25eea27409c8d1599d42b9efd36d695729b1d6/homework8/Party.hs#L47
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel []


-- Exercise 5

fmt :: GuestList -> [String]
fmt (GL employees fun) = ("Total fun: " ++ show fun) : sort (map empName employees)

main :: IO ()
main = do
    txt <- readFile "company.txt"
    let gl = maxFun (read txt :: Tree Employee)
    mapM_ putStrLn $ fmt gl
