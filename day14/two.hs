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

monotone (a:b:xs)
  | a == b = a
  | otherwise = monotone (b:xs)

solve inp = unlines [
  unlines . map show $ inp
  , showMC $ chart
  , showMC . (!!22) $ iterate addSand chart
  , showMC . (!!24) $ iterate addSand chart
  , show . monotone . map (M.size . M.filter (=='o')) $ iterate addSand chart
  ]
  where
    chart :: M.HashMap Coord Char
    chart = toChart M.empty inp
      where
        toChart m (i:is) = toChart (addLines m i) is
        toChart m [] = m

        addLines m [c] = m
        addLines m (c1:c2:cs) = addLines m' (c2:cs)
          where
            coords = getCoords c1 c2

            m' = M.union m $ M.fromList [(c, '#') | c <- coords]

    height = maximum . map snd $ M.keys chart

    addSand :: M.HashMap Coord Char -> M.HashMap Coord Char
    addSand m = fallFrom (500,0)
      where
        fallFrom (x,y)
          | all (`M.member` m) [(x,y+1), (x-1,y+1), (x+1,y+1)] = M.insert (x,y) 'o' m
          | all (`M.member` m) [(x,y+1), (x-1,y+1)] = fallFrom (x+1,y+1)
          | M.member (x,y+1) m = fallFrom (x-1,y+1)
          | y == height+1 = M.insert (x,y) 'o' m
          | otherwise = fallFrom (x,y+1)


    

getCoords c1@(x1,y1) c2@(x2,y2)
  | x1 == x2 && y1 == y2 = [(x1,y1)]
  | x1 == x2 = c1 : getCoords (x1, y1 + signum (y2-y1)) c2
  | y1 == y2 = c1 : getCoords (x1 + signum (x2-x1), y1) c2
