import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Char, Integer)]
readD = readLine `endBy` newline
  where
    readLine = do
      c <- choice (map char "RLUD")
      space
      n <- number
      return (c, n)


solve :: [(Char, Integer)] -> String
solve inp = unlines [
  show inp,
  show anss
  --showMC (M.fromList (zip visited $ repeat '#'))
  ]
  where

    posHs = (0,0) : moveI (0,0) inp

    moveI _ [] = []
    moveI posH (i:is) = posHs ++ moveI (last posHs) is
      where
        posHs = move' i posH
        
        move' ('R', n) (x,y) = [(x+k,y) | k <- [1..n]]
        move' ('L', n) (x,y) = [(x-k,y) | k <- [1..n]]
        move' ('U', n) (x,y) = [(x,y+k) | k <- [1..n]]
        move' ('D', n) (x,y) = [(x,y-k) | k <- [1..n]]

    move _ [] = []
    move tpos (hpos:hs) = tpos' : move tpos' hs
      where
        tpos' = movetail tpos hpos

        movetail (tx, ty) (hx, hy)
          | ty == hy || tx == hx = (movestraight tx hx, movestraight ty hy)
          | abs (ty - hy) + abs (tx - hx) <= 2 = (tx, ty)
          | otherwise = (movediag tx hx, movediag ty hy)
          where
            movestraight x1 x2
              | abs (x1 - x2) <= 1 = x1
              | x1 < x2 = x1+1
              | x1 > x2 = x1-1
            movediag x1 x2
              | x1 < x2 = x1+1
              | x1 > x2 = x1-1
              | otherwise = x1

    visited = (0,0):move (0,0) posHs


    genpos poss = newposs : genpos newposs
      where
        newposs = (0,0):move (0,0) poss

    anss = take 9 . map (length . nub) $ genpos posHs
