import Text.Parsec
import Data.List
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(String, Integer)]
readD = readLine `endBy` newline
  where
    readLine = choice [readnoop, readaddx]

    readnoop = do
      string "noop"
      return ("noop", 0)

    readaddx = do
      string "addx "
      n <- mnumber
      return ("addx", n)


solve inp = unlines [
  show inp,
  show . take 7 $ answerOne,
  show . sum . map (\(c,v) -> c*v) . take 6 $ answerOne
  ] ++ unlines pixm'
  where
    cycs = run 1 1 inp

    run c v (("noop", _):inps) = (c,v):run (c+1) v inps
    run c v (("addx", n):inps) = (c,v):(c+1,v):run (c+2) (v+n) inps
    run c v  [] = [(c,v)]

    answerOne = filter (\(c,v) -> mod (c-20) 40 == 0) cycs

    pixm =  [if abs (v-p) <= 1 then '#' else '.' | (p,v) <- zip (concat (repeat [0..39])) (map snd cycs)]

    pixm' = takes pixm
      where
        takes [] = []
        takes l  = (take 40 l) : takes (drop 40 l)
