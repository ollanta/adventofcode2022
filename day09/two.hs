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
  show answers
  ]
  where
    singleMoves = [d | (d, n) <- inp, _ <- [1..n]]

    headPositions = scanl step (0,0) singleMoves
      where
        step (x,y) 'R' = (x+1,y)
        step (x,y) 'L' = (x-1,y)
        step (x,y) 'U' = (x,y+1)
        step (x,y) 'D' = (x,y-1)

    genTailPositions positions = scanl' follow (0,0) positions
      where
        follow (tx, ty) (hx, hy)
          | abs (tx-hx) <= 1 && abs (ty-hy) <= 1 = (tx, ty)
          | otherwise = (tx + signum (hx-tx), ty + signum (hy-ty))

    tails = iterate genTailPositions headPositions

    answers = take 10 . map (length . nub) $ tails
