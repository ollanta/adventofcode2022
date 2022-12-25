import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = (many1 (oneOf ".#<>^v")) `sepEndBy` newline


solve inp = unlines [
  unlines $ inp
  , show (maxX,maxY)
  --, unlines . map showMC . map combine $ step chart startC
  , show . length $ step chart startC
  ]
  where
    chart :: M.HashMap Coord Char
    chart = readM inp

    startC = (1,0)

    maxY = maximum . map snd . M.keys $ chart
    maxX = maximum . map fst . M.keys $ chart

    step :: M.HashMap Coord Char -> Coord -> [M.HashMap Coord [Char]]
    step ch coord = step' initBlizzs [coord]
      where
        initBlizzs = M.map (:[]) . M.filter (`elem` "<>^v") $ ch

        step' blz coords
          | any ((==maxY) . snd) coords = []
          | otherwise = blz' : step' blz' coords'
          where
            blz' = M.fromListWith (++) [(moveblizz b c, [b]) |
                                        (c,bs) <- M.toList blz,
                                        b <- bs]
            coords' = nub [c' | c <- coords,
                           c' <- c:neighbours c,
                           M.lookupDefault '#' c' ch /= '#',
                           not (M.member c' blz')]

        moveblizz b c = head [c | (x,y) <- tail $ iterate (add delta) c,
                              let c = (x `mod` (maxX+1), y `mod` (maxY+1)),
                                  not (ch M.! c == '#')]
          where
            delta = getBlizzDelta b

    getBlizzDelta b
      | b == '>' = (1,0)
      | b == '<' = (-1,0)
      | b == 'v' = (0,1)
      | b == '^' = (0,-1)

    -- Debug output methods
    cleanMap = M.map (\c -> if c `elem` "<>^v" then '.' else c) chart

    combine blizzards = M.union (M.map showblizz blizzards) cleanMap
      where
        showblizz [b] = b
        showblizz bs
          | length bs > 9 = '*'
          | otherwise = head . show $ length bs
