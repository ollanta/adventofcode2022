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

    minX = minimum . map getX . M.keys $ drop
    maxX = maximum . map getX . M.keys $ drop
    minY = minimum . map getY . M.keys $ drop
    maxY = maximum . map getY . M.keys $ drop
    minZ = minimum . map getZ . M.keys $ drop
    maxZ = maximum . map getZ . M.keys $ drop

    outside = isOutside initM init
      where
        init = concat [[(minX-1,y,z), (maxX+1,y,z)] | y <- [minY..maxY], z <- [minZ..maxZ]] ++ concat [[(x,minY-1,z), (x,maxY+1,z)] | x <- [minX..maxX], z <- [minZ..maxZ]] ++ concat [[(x,y,minZ-1), (x,y,maxZ+1)] | y <- [minY..maxY], x <- [minX..maxX]]

        initM = M.fromList . zip init $ repeat True

        isOutside mem [] = mem
        isOutside mem next = isOutside mem' next'
          where
            cneigh = [add out side |
                      out <- next,
                      side <- neighbours]
            
            next' = nub [c |
                     c@(x,y,z) <- cneigh,
                     x >= minX && x <= maxX,
                     y >= minY && y <= maxY,
                     z >= minZ && z <= maxZ,
                     not (M.member c drop),
                     not (M.member c mem)]

            mem' = M.union mem (M.fromList $ zip next' (repeat True))


    contactless = [cube | cube <- inp,
                   side <- neighbours,
                   M.member (add cube side) outside
                   ]
