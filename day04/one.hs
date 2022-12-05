import Text.Parsec
import Data.List
import Data.Char
import Parsing

main :: IO ()
main = optimisticInteract readD (show . solve)


readD :: Parser [((Integer, Integer),(Integer, Integer))]
readD = readLine `endBy` newline
  where
    readLine = do
      r1 <- readRange
      char ','
      r2 <- readRange
      return (r1, r2)

    readRange = do
      n1 <- number
      char '-'
      n2 <- number
      return (n1, n2)

solve :: [String] -> Int
solve l = sum . map contained $ l
  where
    contained :: ((Integer, Integer), (Integer, Integer)) -> Integer
    contained (r1, r2)
      | cont' r1 r2 = 1
      | cont' r2 r1 = 1
      | otherwise   = 0
      where
        cont' (s1, e1) (s2, e2) = s1 <= s2 && e1 >= e2
