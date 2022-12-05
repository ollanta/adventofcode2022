import Data.List
import Data.Char

main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Int
solve l = sum . map (contained . readl) $ l
  where
    readp :: String -> (Integer, Integer)
    readp s = (read s1, read s2)
      where
        s1 = takeWhile (/='-') s
        s2 = tail $ dropWhile (/='-') s

    readl s = (readp s1, readp s2)
      where
        s1 = takeWhile (/=',') s
        s2 = tail $ dropWhile (/=',') s

    contained :: ((Integer, Integer), (Integer, Integer)) -> Integer
    contained (r1, r2)
      | cont' r1 r2 = 1
      | otherwise   = 0
      where
        cont' (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1
