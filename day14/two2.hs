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

solve inp = unlines [
  show . countSand $ findSteady chart
  ]
  where
    chart :: M.HashMap Coord Char
    chart = M.fromList stones
      where
        stones = [ (c, '#') | shape <- inp, line <- zip shape (tail shape), c <- getCoords line]

    height = maximum . map snd $ M.keys chart

    findSteady :: M.HashMap Coord Char -> M.HashMap Coord Char
    findSteady m = fst $ addSand (500,0) m
      where
        addSand c@(x,y) m
          | isGround  = (m, True)
          | holdsSand = (M.insert c 'o' m', True)
          | otherwise = (m', False)
          where
            isGround = y == height+2 || M.member c m
            (m', holdsSand) = foldl' conditionalFlow (m,True) [(nx, y+1) | nx <- [x, x-1, x+1]]

        conditionalFlow (m, willFlow) c
          | willFlow  = addSand c m
          | otherwise = (m, False)

    countSand = M.size . M.filter (=='o')


getCoords (c1@(x1,y1), c2@(x2,y2))
  | c1 > c2  = getCoords (c2, c1)
  | x1 == x2 = [(x1, y) | y <- [y1..y2]]
  | y1 == y2 = [(x, y1) | x <- [x1..x2]]
