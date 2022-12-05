import Data.List
import Data.Char

main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Int
solve l = sum . map (priority . common3) . elf $ l
  where
    elf (s1:s2:s3:ss) = (s1, s2, s3):elf ss
    elf _ = []

    common3 (a, b, c) = head $ common (a, common (b, c))
    common (a, b) = filter (`elem` nub b) (nub a)

    priority c
      | ord c >= ord 'a' = ord c - ord 'a' + 1
      | otherwise        = ord c - ord 'A' + 27
