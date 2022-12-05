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
        toVisibleRow v (t:ts)
          | v < t = True : toVisibleRow t ts
          | otherwise = False : toVisibleRow v ts
        toVisibleRow _ [] = []

    inps = [toVisibleChart inp,
            (transpose) (toVisibleChart $ transpose inp),
            (map reverse) (toVisibleChart $ (map reverse) inp),
            (transpose . map reverse) (toVisibleChart $ (map reverse . transpose) inp)]

    allcharts = map readM inps

    unionchart = foldl1 (M.unionWith (||)) allcharts

    
