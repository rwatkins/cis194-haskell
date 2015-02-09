module Golf where

nth :: [a] -> Int -> [a]
nth [] _ = []
nth c n
    | n < 2     = c
    | otherwise = take 1 (drop (n-1) c) ++ nth (drop n c) n

skips :: [a] -> [[a]]
skips x = s x $ length x
    where s c 0 = []
          s c n = s c (n-1) ++ [nth c n]

-- Triples
tr :: [a] -> [[a]]
tr (w:x:y:z) = [[w, x, y]] ++ tr (x:y:z)
tr _         = []

m :: Ord a => [a] -> Bool
m [x, y, z] = y > x && y > z
m _         = False

localMaxima :: Ord a => [a] -> [a]
localMaxima [] = []
localMaxima x = map m' $ filter m $ tr x
    where m' [_, y, _] = y

histogram :: [Integer] -> String
histogram xs =   -- For each number from 0 to 9, count the instances of
                 -- that number in xs by filtering on equality to it and
                 -- getting length length of each resulting list.
                 -- toInteger is required because otherwise it's a list
                 -- of Int instead.
    let counts = map (toInteger . (\y -> length $ filter (==y) xs)) [0..9]
        bars = concat $ map (line counts) $ reverse [1..maximum counts]
    in bars ++ "==========\n0123456789\n"
          -- Produces a line of stars and spaces
    where line ints n = map (star n) ints ++ ['\n']
          -- Returns a star or a space
          star n x = if x >= n then '*' else ' '
