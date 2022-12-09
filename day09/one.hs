import Text.Parsec
import Data.List
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Char, Integer)]
readD = readLine `endBy` newline
  where
    readLine = do
      c <- oneOf "RLUD"
      space
      n <- number
      return (c, n)


solve :: [(Char, Integer)] -> String
solve inp = unlines [
  show inp,
  show $ length (nub tailPositions),
  --showMC (M.fromList (zip headPositions $ repeat '#')),
  --showMC (M.fromList (zip tailPositions $ repeat '#'))
  ]
  where

    singleMoves = [d | (d, n) <- inp, _ <- [1..n]]

    headPositions = scanl step (0,0) singleMoves
      where
        step (x,y) 'R' = (x+1,y)
        step (x,y) 'L' = (x-1,y)
        step (x,y) 'U' = (x,y+1)
        step (x,y) 'D' = (x,y-1)

    tailPositions = scanl follow (0,0) headPositions
      where
        follow (tx, ty) (hx, hy)
          | abs (tx-hx) <= 1 && abs (ty-hy) <= 1 = (tx, ty)
          | otherwise = (tx + signum (hx-tx), ty + signum (hy-ty))
