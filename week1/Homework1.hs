toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits i
  | i < 0 = []
  | otherwise = [(read [s]) | s <- (show i)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [i] = [i]
doubleEveryOther (i:j:ks)
  | (length ks) `mod` 2 == 1 = i : 2*j : doubleEveryOther ks
  | otherwise                = 2*i : j : doubleEveryOther ks

sumDigits :: [Integer] -> Integer
sumDigits ints = foldr (+) 0 [read [c] | c <- intsToString ints]
  where intsToString []     = ""
        intsToString (i:js) = show i ++ intsToString js

validate :: Integer -> Bool
validate i = let result = sumDigits . doubleEveryOther . toDigits $ i
             in result `mod` 10 == 0

main :: IO ()
main = do
    putStrLn "toDigits"
    putStrLn $ show $ toDigits 0
    putStrLn $ show $ toDigits 12345
    putStrLn $ show $ toDigits (-17)

    putStrLn "doubleEveryOther"
    putStrLn $ show $ doubleEveryOther [1, 2]
    putStrLn $ show $ doubleEveryOther [1, 2, 3, 4, 5, 6, 7]
    putStrLn $ show $ doubleEveryOther [1, 2, 3, 4, 5, 6, 7, 8]

    putStrLn "sumDigits []"
    putStrLn $ show $ sumDigits []
    putStrLn "sumDigits [12345]"
    putStrLn $ show $ sumDigits [12345]
    putStrLn "sumDigits [10, 11, 12]"
    putStrLn $ show $ sumDigits [10, 11, 12]

    putStrLn "validate 4012888888881881 = True"
    putStrLn $ show $ if (validate 4012888888881881)
                      then "PASS"
                      else "FAIL"
    putStrLn "validate 4012888888881882 = False"
    putStrLn $ show $ if not (validate 4012888888881882)
                      then "PASS"
                      else "FAIL"
