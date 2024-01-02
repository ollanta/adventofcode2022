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
  --, unlines $ map showMC rockMs
  --, unlines . reverse . lines $ showMC states
  , unlines . reverse . lines . showMC $ states !! 7656
  , show . maximum . map snd . M.keys $ states !! 4499
  --, unlines . map show $ states
  , unlines . map show . take 20 $ finddupes states
  , show $ getMaxYAt 1000000000000
  ]
  where
    start = M.fromList $ [((x,0), '-') | x <- [-1..8]]

    rocks = [[(x,0) | x <- [0..3]],
              [(1,0), (0,1), (1,1), (2,1), (1,2)],
              [(0,0), (1,0), (2,0), (2,1), (2,2)],
              [(0,0), (0,1), (0,2), (0,3)],
              [(0,0), (0,1), (1,0), (1,1)]]

    rockMs = [M.fromList (zip rock (repeat '#')) | rock <- rocks]

    allrocks = concat . repeat $ zip [1..] rockMs
    jets = concat . repeat $ zip [1..] inp

    step world ((ri,rock):rocks) ((ji,jet):jets) (x,y)
      | settles = world'':step world'' rocks ((ji,jet):jets) (2,maxY+5)
      | otherwise = step world ((ri,rock):rocks) jets (x',y-1)
      where
        -- falldown
        settles = any (`M.member` world) [(x+dx,y+dy-1) | (dx,dy) <- M.keys rock]
        world' = (M.union world $ M.fromList [((x+dx,y+dy),'#') | (dx,dy) <- M.keys rock])
        maxY = maximum . map snd $ M.keys world'
        world'' = M.unions [world', M.fromList [((-1,y), '|') | y <- [maxY-4..maxY]], M.fromList [((7,y),'|') | y <- [maxY-4..maxY]]]

        push '<'
          | x == 0 = 0
          | otherwise = -1
        push '>'
          | x == 6 - (maximum . map fst $ M.keys rock) = 0
          | otherwise = 1

        collides = any (`M.member` world) [(x+dx+push jet,y+dy-1) | (dx,dy) <- M.keys rock]
        x' = x + if collides then 0 else push jet

    states :: [M.HashMap (Integer,Integer) Char]
    states = step start allrocks jets (2,5)

    finddupes sts = finddupes' M.empty (zip [1..] sts)
      where
        finddupes' visited ((i,st):sts)
          | M.member last1k visited = (i, maxY, visited M.! last1k):finddupes' visited sts
          | otherwise               = finddupes' visited' sts
          where
            maxY = maximum . map snd . M.keys $ st
            --last1k = [(x,maxY-y) | (x,y) <- M.keys st, y >= maxY-15]
            last1k = [(x,dy) | x <- [0..6], dy <- [0..100], M.lookup (x,maxY-dy) st == Just '#']
            visited' = M.insert last1k (i,maxY) visited

    
    getMaxYAt k = maxY1 + q * (maxY2-maxY1) + maxYdelta r
      where
        dupes = finddupes states
        (cycle2Start, maxY2, (cycle1Start, maxY1)) = head dupes
        cycleLength = cycle2Start-cycle1Start
        
        (q, r) = (k-cycle1Start) `quotRem` cycleLength

        -- could get this from the first cycle, but eh
        maxYdelta r = getMaxY (dupes !! fromInteger r) - maxY2
          where
            getMaxY (_, maxY, _) = maxY
