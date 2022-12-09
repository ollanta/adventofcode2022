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
  showMS chart,
  showMS scorechart,
  show ans
  ]
  where
    chart = readM inp

    width = toInteger $ length (head inp)
    height = toInteger $ length inp

    scenicdistance (x,y) (dx, dy) v
      | null rest = length lowertrees
      | otherwise = length lowertrees + 1 -- we see the last tree too
      where
        coords = [(x+n*dx, y+n*dy) | n <- [1..]]
        trees = [chart M.! c | c <- takeWhile (`M.member` chart) coords]

        (lowertrees, rest) = span (<v) trees

    scenicscore c v = product [scenicdistance c dir v |
                               dir <- [(1,0), (0,1), (-1,0), (0,-1)]]

    scorechart = M.mapWithKey scenicscore chart

    ans = maximum . M.elems $ scorechart
