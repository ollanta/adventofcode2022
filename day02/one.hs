
main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Integer
solve l = sum $ zipWith (+) (map outcome l) (map me l)
  where
    me (_:_:c:[])
      | c == 'X' = 1
      | c == 'Y' = 2
      | c == 'Z' = 3

    outcome "A X" = 3
    outcome "A Y" = 6
    outcome "A Z" = 0
    outcome "B X" = 0
    outcome "B Y" = 3
    outcome "B Z" = 6
    outcome "C X" = 6
    outcome "C Y" = 0
    outcome "C Z" = 3
