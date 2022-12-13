import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[Char]]
readD = readLine `endBy` newline
  where
    readLine = do
      s <- many alphaNum
      return s
      

solve inp = unlines [
  show (solve chart)
  ]
  where
    chart = readM inp

    visited = M.empty

    solve m = step 0 visited s
      where
        s = M.keys . M.filter (=='a') $ m
        
        step k _ [] = []
        step k vis cs
          | not . null . filter (=='E') $ vs = [(k,cs,vs)]
          | otherwise = step (k+1) vis' (nub cs')
          where
            vs = map (m M.!) cs

            cs' = [c' | c <- cs, c' <- neighs c, isNext (m M.! c) (m M.! c'), not (c' `M.member` vis)]

            vis' = M.union vis (M.fromList $ zip cs' (repeat True))
          
        neighs (x,y) = filter (`M.member` m) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

    isNext 'z' c = c == 'E' || ord c <= ord 'z'
    isNext 'S' c = c == 'a'
    isNext v c
      | isLower c = ord c <= ord v + 1
      | otherwise = c == 'S'
