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
  , show $ ans
  , show (fx * 4000000 + fy)
  ]
  where
    ans = search (0,0)
    Just (fx,fy) = ans

    maxc = 4000000 -- for real input
    --maxc = 20 -- for test input

    search c@(x,y)
      | x >= maxc = search (0, y+1)
      | y >  maxc = Nothing
      | null cCandidates = Just c
      | otherwise = search $ maximum cCandidates
      where
        overlapping = [ (sc, bc) | (sc,bc) <- inp, manhattan sc c <= manhattan sc bc ]
        cCandidates = [ pushRight c o | o <- overlapping ]


    pushRight (x,y) (sc@(sx,sy), bc) = (sx + dx,y)
      where
        dx = 1 + manhattan sc bc - manhattan sc (sx, y)


manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs(y1-y2)
