import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[Integer]]
readD = readLine `endBy` newline
  where
    readLine = many1 digitAsNumber


solve inp = unlines [
  show inp,
  show chart,
  showM unionchart (\c -> if c then "x" else " "),
  show . M.size . M.filter (==True) $ unionchart
  ]
  where
    chart = readM inp

    toVisibleChart :: [[Integer]] -> [[Bool]]
    toVisibleChart rows = map (toVisibleRow (-1)) rows
      where
        toVisibleRow v (t:ts) = (v < t) : toVisibleRow (max t v) ts
        toVisibleRow _ [] = []

    rotate :: Int -> [[a]] -> [[a]]
    rotate 0 rows = rows
    rotate n rows = rotate (n-1) rotated
      where
        rotated = foldr (zipWith (:)) (repeat []) (reverse rows)

    rotations = [rotate n . toVisibleChart . rotate (4-n) $ inp | n <- [0..3]]

    unionchart = foldl1 (M.unionWith (||)) $ map readM rotations
