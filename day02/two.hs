
main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Integer
solve l = sum $ zipWith (+) (map outcome l) (map me l)
  where
    outcome (_:_:c:[])
      | c == 'X' = 0
      | c == 'Y' = 3
      | c == 'Z' = 6

    me "A X" = 3
    me "A Y" = 1
    me "A Z" = 2

    me "B X" = 1
    me "B Y" = 2
    me "B Z" = 3

    me "C X" = 2
    me "C Y" = 3
    me "C Z" = 1
