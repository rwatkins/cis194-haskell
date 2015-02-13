{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add left right) = (eval left) + (eval right)
eval (ExprT.Mul left right) = (eval left) * (eval right)


-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul s) of
              Just expr -> Just (eval expr)
              Nothing   -> Nothing


class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul


-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7


-- Exercise 5

instance Expr Program where
  lit i = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile s = case parse s of
              Just p  -> Just p
              Nothing -> Nothing
  where parse = parseExp lit add mul


-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Eq, Show)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- No idea if any of this is correct.
-- Borrowed from http://stackoverflow.com/a/24911354/52263

instance Expr (M.Map String Integer -> Maybe Integer) where
  add m n = o
    where o ms2i = let i1 = m ms2i
                       i2 = n ms2i
                   in case (i1, i2) of
                        (Just i, Just j) -> Just $ i + j
                        _                -> Nothing
  mul m n = o
    where o ms2i = let i1 = m ms2i
                       i2 = n ms2i
                   in case (i1, i2) of
                        (Just i, Just j) -> Just $ i * j
                        _                -> Nothing
  lit i = o
    where o ms2i = Just i


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
