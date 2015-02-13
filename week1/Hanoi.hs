type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
  | n <= 0    = []
  | n == 1    = [(p1, p2)]
  | otherwise = (hanoi (n-1) p1 p3 p2) ++
                (hanoi 1 p1 p2 p3) ++
                (hanoi (n-1) p3 p2 p1)

main :: IO ()
main = do
  putStrLn . show . length $ hanoi 15 "a" "b" "c"
  putStrLn . show $ hanoi 15 "a" "b" "c"
