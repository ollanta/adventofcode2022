import Text.Parsec
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = (many1 (oneOf ".#")) `sepEndBy` newline


solve inp = unlines [
  show $ inp
  --, unlines . map show . take 5 $ step chart
  , unlines . map showMC . take 10 $ step chart moves
  , show $ getAns final
  ]
  where
    final = head . drop 10 $ step chart moves

    getAns ch = (maxY-minY+1)*(maxX-minX+1)-(toInteger $ length elves)
      where
        elves = M.keys $ M.filter (=='#') ch
        minX = minimum . map fst $ elves
        minY = minimum . map snd $ elves
        maxX = maximum . map fst $ elves
        maxY = maximum . map snd $ elves

    chart = readM inp
    moves = concat . repeat $ [(0,-1),(0,1),(-1,0),(1,0)]

    step ch mvs = ch : step ch' (tail mvs)
      where
        elves = M.filter (=='#') ch

        adjelves = [elf |
                    elf <- M.keys elves,
                    any (=='#') [M.lookupDefault '.' neigh ch | neigh <- neighbours8 elf]]

        consider (dx,dy) elf@(ex,ey) = all (=='.') [M.lookupDefault '.' (x',y') ch | x' <- toRange dx ex, y' <- toRange dy ey]
          where
            -- Ease generation of all neighbours to the E/W/N/S
            toRange 0 x = [x-1..x+1]
            toRange k x = [x+k]
            
        getMove elf = head $ [ add elf delta | delta <- take 4 mvs, consider delta elf] ++ [elf]

        propmoves = [(elf, move) | elf <- adjelves, let move = getMove elf, move /= elf]

        dupemoves = M.fromListWith (\_ _ -> True) [(prop, False) | (_, prop) <- propmoves]

        movedelves = M.fromList $ concat [[(elf, '.'),(prop, '#')] | (elf, prop) <- propmoves, dupemoves M.! prop == False]

        ch' = M.union movedelves ch
