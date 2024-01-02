import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser String
readD = readLine
  where
    readLine = many1 (oneOf "<>")


solve inp = unlines [
  show $ inp
  , unlines $ map showMC rockMs
  , show $ maximum . map snd $ M.keys final
  ]
  where
    start = M.fromList [((x,0), '-') | x <- [-1..8]]

    rocks = [[(x,0) | x <- [0..3]],
              [(1,0), (0,1), (1,1), (2,1), (1,2)],
              [(0,0), (1,0), (2,0), (2,1), (2,2)],
              [(0,0), (0,1), (0,2), (0,3)],
              [(0,0), (0,1), (1,0), (1,1)]]

    rockMs = [M.fromList (zip rock (repeat '#')) | rock <- rocks]

    allrocks = take 2022 . concat $ repeat rockMs

    step world [] _ _ = world
    step world (rock:rocks) (jet:jets) (x,y)
      | settles = step world' rocks (jet:jets) (2,maxY+5)
      | otherwise = step world (rock:rocks) jets (x',y-1)
      where
        -- falldown
        settles = any (`M.member` world) [(x+dx,y+dy-1) | (dx,dy) <- M.keys rock]
        world' = (M.union world $ M.fromList [((x+dx,y+dy),'#') | (dx,dy) <- M.keys rock])
        maxY = maximum . map snd $ M.keys world'

        push '<'
          | x == 0 = 0
          | otherwise = -1
        push '>'
          | x == 6 - (maximum . map fst $ M.keys rock) = 0
          | otherwise = 1

        collides = any (`M.member` world) [(x+dx+push jet,y+dy-1) | (dx,dy) <- M.keys rock]
        x' = x + if collides then 0 else push jet
        

    final = step start allrocks (concat . repeat $ inp) (2,5)
