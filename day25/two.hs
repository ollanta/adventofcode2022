import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = (many1 (oneOf ".#<>^v")) `sepEndBy` newline


solve inp = unlines [
  show $ inp
  , show (maxX,maxY)
  , show . length $ step chart startC
  --, unlines . map showMC . map combine $ step chart startC
  --, unlines . map show $ step chart startC
  ]
  where
    ans = 1

    chart :: M.HashMap Coord Char
    chart = readM inp

    startC = (1,0)

    maxY = maximum . map snd . M.keys $ chart
    maxX = maximum . map fst . M.keys $ chart

    cleanMap = M.map (\c -> if c `elem` "<>^v" then '.' else c) chart

    combine blm = M.union (M.map toc blm) cleanMap
      where
        toc [b] = b
        toc l   = head . show . length $ l

    --step :: M.HashMap Coord Char -> Coord -> Integer
    step ch coord = step' [maxY,0,maxY] 0 bl [coord]
      where
        bl = M.map (:[]) . M.filter (`elem` "<>^v") $ ch

        step' [] minute blz coords = []
        step' (goaly:goals) minute blz coords
          | any ((==goaly) . snd) coords = step' goals minute blz (filter ((==goaly) . snd) coords)
        step' goals minute blz coords
          | otherwise = blz:step' goals (minute+1) blz' coords'
          where
            blz' = M.fromListWith (++) [(moveb b c, [b]) | (c,bs) <- M.toList blz, b <- bs]
            coords' = nub [c' | c <- coords,
                           c' <- c:neighbours c,
                           M.lookupDefault '#' c' ch /= '#',
                           not (M.member c' blz')]

            
        moveb b c = head . dropWhile isWall . map cap . tail $ iterate (add d) c
          where
            d = getD b
            cap (x,y) = (x `mod` (maxX+1), y `mod` (maxY+1))
            isWall c = ch M.! c == '#'

        getD '>' = (1,0)
        getD '<' = (-1,0)
        getD 'v' = (0,1)
        getD '^' = (0,-1)
