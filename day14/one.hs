import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[(Integer,Integer)]]
readD = readLine `sepEndBy` newline
  where
    readLine = readCoord `sepBy1` string " -> "

    readCoord = do
      n1 <- number
      char ','
      n2 <- number
      return (n1, n2)

findSteady (a:b:bs)
  | a == b = a
  | otherwise = findSteady (b:bs)

solve inp = unlines [
  show . countSand . findSteady $ states
  ]
  where
    chart :: M.HashMap Coord Char
    chart = M.fromList stones
      where
        stones = [ (c, '#') | shape <- inp, line <- zip shape (tail shape), c <- getCoords line]

    height = maximum . map snd $ M.keys chart

    addSand :: Coord -> M.HashMap Coord Char -> M.HashMap Coord Char
    addSand (x,y) m
      | y > height      = m
      | null nextCoords = M.insert (x,y) 'o' m
      | otherwise       = addSand nextCoord m
      where
        nextCoords = [ nc | nx <- [x, x-1, x+1], let nc = (nx, y+1), not (M.member nc m)]
        nextCoord  = head nextCoords

    states = iterate (addSand (500,0)) chart

    countSand = M.size . M.filter (=='o')

getCoords (c1@(x1,y1), c2@(x2,y2))
  | c1 > c2  = getCoords (c2, c1)
  | x1 == x2 = [(x1, y) | y <- [y1..y2]]
  | y1 == y2 = [(x, y1) | x <- [x1..x2]]
