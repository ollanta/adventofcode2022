
main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Integer
solve l = maximum (map sum $ parts l)
  where
    parts :: [String] -> [[Integer]]
    parts [] = []
    parts ("":ls) = parts ls
    parts ls = (map read $ takeWhile (/= "") ls) : parts (dropWhile (/="") ls)
