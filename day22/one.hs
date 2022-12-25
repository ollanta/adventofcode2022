import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

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
  show final
  --, unlines . map show $ allcoords
  , show $ getPassword final
  ]
  where
    allcoords = move startcoord startdir chart rawcode
    final = last allcoords
    getPassword ((fx,fy),fd) = 1000*fy+4*fx+dirToInt fd

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

        move' (Forward n) = (movedir coord coord n dir, dir)
        move' (Turn c) = (coord, rotate c dir)

        movedir c lc 0 d = c
        movedir c lc k d
          | obj == '#' = lc
          | obj == ' ' = movedir c' lc k d
          | obj == '.' = movedir c' c' (k-1) d
          where
            c' = cap $ add c (dirToDelta d)
            obj = M.lookupDefault ' ' c' ch

    cap (x,y) = (x `mod` (maxX+1), y `mod` (maxY+1))

    dirTranslator = zip3 [FR,FD,FL,FU] [(1,0),(0,1),(-1,0),(0,-1)] [0..3]

    dirToDelta dir = head [delta | (d, delta, _) <- dirTranslator, d == dir]
    dirToInt dir   = head [i | (d, _, i) <- dirTranslator, d == dir]

    rotate 'R' dir = head [d | (d, _, i) <- dirTranslator, i == (dirToInt dir + 1) `mod` 4]
    rotate 'L' dir = head [d | (d, _, i) <- dirTranslator, i == (dirToInt dir - 1) `mod` 4]
