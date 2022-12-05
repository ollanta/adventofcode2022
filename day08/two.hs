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
      | length trees > lowertrees = lowertrees + 1
      | otherwise             = lowertrees
      where
        dir = [(x+n*dx, y+n*dy) | n <- [1..]]
        cs = takeWhile (\(x,y) -> 0 <= x && x < width && 0 <= y && y < height) dir

        trees = map (chart M.!) cs

        lowertrees = length $ takeWhile (<v) trees

    scenicscore c v = product [scenicdistance c (1,0) v,
                               scenicdistance c (0,1) v,
                               scenicdistance c (-1,0) v,
                               scenicdistance c (0,-1) v]

    scorechart = M.mapWithKey (scenicscore) chart

    scorelist = M.toList scorechart

    ans = maximum . map snd $ scorelist
