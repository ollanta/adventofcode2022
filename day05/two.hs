import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve


readD :: Parser ([[[Char]]], [Integer], [(Integer, Integer, Integer)])
readD = do
  r1 <- read1
  r2 <- read2
  newline
  newline
  r3 <- read3
  return (r1, r2, r3)
  where
    readBox = choice [empty, box] `sepBy1` (char ' ')
      where
        empty = do
          try (string "   ")
          return ' '

        box = do
          char '['
          c <- letter
          char ']'
          return c

    read1 = readl `endBy` newline
      where
        readl = many1 readBox

    readN = do
      try space
      n <- number
      space
      return n

    read2 = readN `sepBy1` (char ' ')

    read3 = readInstr `endBy` newline
      where
        readInstr = do
          string "move "
          n <- number
          string " from "
          c1 <- number
          string " to "
          c2 <- number
          return (n, c1, c2)


solve :: ([[[Char]]], [Integer], [(Integer,Integer,Integer)]) -> String
solve (boxes, numbers, moves) = unlines [show moves, show stacks', answer]
  where
    n = length numbers

    boxes' = reverse $ map (\b -> take n $ zip [1..] (head b ++ repeat ' ')) boxes

    putStack m (i,' ') = m
    putStack m (i,c) = M.alter alt i m
      where
        alt Nothing = Just [c]
        alt (Just l) = Just (c:l)

    
    genStacks :: M.HashMap Integer [Char] -> [[(Integer,Char)]] -> M.HashMap Integer [Char]
    genStacks m [] = m
    genStacks m (b:bs) = genStacks m' bs
      where
        m' = foldl' putStack m b
        
    initStacks = genStacks M.empty boxes'

    stacks' = runMoves initStacks moves
      where
        runMoves st [] = st
        runMoves st (mv:mvs) = runMoves (runMoves' st mv) mvs
          where
            runMoves' st (n, from, to) = st''
              where
                i = fromInteger n
                s = take i $ st M.! from
                st' = M.adjust (drop i) from st
                st'' = M.adjust (s++) to st'

    answer = map (head . snd) . sort $ M.toList stacks'
