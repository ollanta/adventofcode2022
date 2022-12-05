import Data.List
import Data.Char

main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Int
solve l = sum . map (priority . common . backpack) $ l
  where
    backpack :: String -> (String, String)
    backpack s = (take l s, drop l s)
      where l = length s `div` 2

    
    common (a, b) = head $ filter (`elem` nub b) (nub a)

    priority c
      | ord c >= ord 'a' = ord c - ord 'a' + 1
      | otherwise        = ord c - ord 'A' + 27
