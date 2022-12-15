import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [((Integer,Integer),(Integer,Integer))]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      string "Sensor at "
      sensor <- readXY
      string ": closest beacon is at "
      beacon <- readXY
      return (sensor, beacon)

    readXY = do
      string "x="
      x <- mnumber
      string ", y="
      y <- mnumber
      return (x,y)


solve inp = unlines [
  show $ inp
  , show $ (M.size ans - length beaconsAt)
  ]
  where
    ans = foldl' checkCant M.empty inp

    beaconsAt = nub [ (bx,by) | (_,(bx,by)) <- inp, by == ty]

    --ty = 10
    ty = 2000000

    checkCant m (sc@(sx,sy),bc) = M.union m $ M.fromList (zip newCant (repeat True))
      where
        dist = manhattan sc bc

        newCant = [(x, ty) | x <- [sx-dist..sx+dist], manhattan sc (x,ty) <= dist]


manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs(y1-y2)
