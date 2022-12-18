import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart3d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer,Integer,Integer)]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      x <- number
      string ","
      y <- number
      string ","
      z <- number
      return (x,y,z)


solve inp = unlines [
  show $ length contactless
  ]
  where
    drop = M.fromList $ zip inp (repeat True)

    neighbours = [(1,0,0),(-1,0,0),
                 (0,1,0),(0,-1,0),
                 (0,0,1),(0,0,-1)]

    contactless = [cube | cube <- inp,
                   side <- neighbours,
                   not (M.member (add cube side) drop)
                   ]
