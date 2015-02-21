{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int deriving (Eq, Num, Ord, Show)

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

score :: Char -> Score
score c | u `elem` "AEILNORSTU" = Score 1
        | u `elem` "DG"         = Score 2
        | u `elem` "BCMP"       = Score 3
        | u `elem` "FHVWY"      = Score 4
        | u `elem` "K"          = Score 5
        | u `elem` "JX"         = Score 8
        | u `elem` "QXZ"        = Score 10
    where u = toUpper c

scoreString :: String -> Score
scoreString s = mconcat $ map score s
