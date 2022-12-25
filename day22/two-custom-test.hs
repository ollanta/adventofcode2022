import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

{-
Custom-implemented cube continuity for the folding layout in test.txt
-}

main :: IO ()
main = optimisticInteract readD solve

data Face = FL | FR | FU | FD
  deriving (Eq, Show)

data Instruction = Forward Integer | Turn Char
  deriving (Eq, Show)

readD :: Parser ([String], [Instruction])
readD = do
  chart <- readChart
  newline
  code <- many1 readCode
  return (chart, code)
  where
    readChartLine = many1 (oneOf " .#")
    readChart = readChartLine `endBy` newline

    readCode = choice [Forward <$> number, Turn <$> oneOf "RL"]


solve (rawchart, rawcode) = unlines [
  show cubeside
  , unlines . map show $ take 50 testCases
  --, unlines . map show $ allcoords
  , show $ getPassword final
  ]
  where
    allcoords = move startcoord startdir chart rawcode
    final = last allcoords
    getPassword ((fx,fy),fd) = 1000*fy+4*fx+dirToInt fd

    cubeside = maxY `div` 3

    chart = M.filter (/=' ') . M.fromList . map (\((x,y),c) -> ((x+1,y+1),c)) . M.toList $ readM rawchart

    maxY = toInteger $ length rawchart
    maxX = fst . maximum $ M.keys chart

    startcoord = minimum . filter (\(x,y) -> y == 1) . M.keys $ M.filter (=='.') chart
    startdir = FR

    move :: Coord -> Face -> M.HashMap Coord Char -> [Instruction] -> [(Coord, Face)]
    move coord dir ch [] = [(coord, dir)]
    move coord dir ch (mv:mvs) = (coord,dir) : move coord' dir' ch mvs
      where
        (coord', dir') = move' mv

        move' (Forward n) = movedir coord coord n dir
        move' (Turn rl) = rotate rl (coord, dir)

        movedir c lc 0 d = (c,d)
        movedir c lc k d
          | obj == '#' = (lc,d)
          | obj == '.' = movedir c' c' (k-1) d'
          where
            (c',d') = stepOnCube (c, d)
            obj = ch M.! c'

    stepOnCube (c, d) = locateOnCube d $ add c (dirToDelta d)
    rotate rl  (c, d) = (c, rotateDir rl d)

    locateOnCube d (x,y)
      | d == FL && xs == 1 && ys == 0 = ((cubeside+y, cubeside+1),FD)
      | d == FR && xs == 3 && ys == 0 = ((4*cubeside, 3*cubeside+1-y),FL)
      | d == FU && xs == 2 && y  == 0 = ((cubeside+1-cx, cubeside+1),FD)

      | d == FU && xs == 0 && ys == 0 = ((3*cubeside+1-cx,1),FD)
      | d == FL && x  == 0 && ys == 1 = ((4*cubeside+1-cy,3*cubeside),FU)
      | d == FD && xs == 0 && ys == 2 = ((3*cubeside+1-cx,3*cubeside),FU)

      | d == FU && xs == 1 && ys == 0 = ((2*cubeside+1, cx),FR)
      | d == FD && xs == 1 && ys == 2 = ((2*cubeside+1, 3*cubeside+1-cx), FR)

      | d == FR && xs == 3 && ys == 1 = ((4*cubeside+1-cy, 2*cubeside+1), FD)

      | d == FL && xs == 1 && ys == 2 = ((2*cubeside+1-cy,2*cubeside), FU)
      | d == FD && xs == 2 && ys == 3 = ((cubeside+1-cx,2*cubeside), FU)

      | d == FU && xs == 3 && ys == 1 = ((3*cubeside, 2*cubeside+1-cx), FL)
      | d == FR && xs == 4 && ys == 2 = ((3*cubeside, cubeside+1-cy), FL)
      | d == FD && xs == 3 && ys == 3 = ((1, 2*cubeside+1-cx), FR)

      | otherwise = ((x,y),d)
      where
        cx = ((x-1) `mod` cubeside) + 1
        cy = ((y-1) `mod` cubeside) + 1
        xs = (x-1) `div` cubeside
        ys = (y-1) `div` cubeside
            
    dirTranslator = zip3 [FR,FD,FL,FU] [(1,0),(0,1),(-1,0),(0,-1)] [0..3]

    dirToDelta dir = head [delta | (d, delta, _) <- dirTranslator, d == dir]
    dirToInt dir   = head [i | (d, _, i) <- dirTranslator, d == dir]

    rotateDir 'R' dir = head [d | (d, _, i) <- dirTranslator, i == (dirToInt dir + 1) `mod` 4]
    rotateDir 'L' dir = head [d | (d, _, i) <- dirTranslator, i == (dirToInt dir - 1) `mod` 4]
            

    -- Test cases to make sure locateOnCube is sensible
    -- should be empty if all is good

    testCases = [(((x,y), dir), forward, turnedBack) |
                 x <- [1..4*cubeside],
                 y <- [1..4*cubeside],
                 let xs = (x-1) `div` cubeside,
                 let ys = (y-1) `div` cubeside,
                 elem (xs,ys) [(2,0),(0,1),(1,1),(2,1),(2,2),(3,2)], -- coord on cube
                 dir <- [FR,FL,FU,FD],
                 let forward = stepOnCube ((x,y), dir),
                 let turnedAround = rotate 'R' $ rotate 'R' forward,
                 let movedBack = stepOnCube turnedAround,
                 let turnedBack = rotate 'R' $ rotate 'R' movedBack,
                 turnedBack /= ((x,y),dir)]
