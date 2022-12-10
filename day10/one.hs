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
    readLine = do
      command <- many1 letter
      many (char ' ')
      number <- option 0 mnumber
      return (command, number)

solve inp = unlines [
  show . sum . take 6 $ answerOne
  ] ++ unlines (splitlines printed)
  where
    cycs = run 1 inp

    run v (("noop", _):inps) = v:run v inps
    run v (("addx", n):inps) = v:v:run (v+n) inps
    run v [] = [v]

    answerOne = [ c*v | (c,v) <- zip [1..] cycs, mod (c-20) 40 == 0]

    printed = map draw $ zip [0..] cycs
      where
        draw (p,v)
          | abs (mod p 40 - v) <= 1 = '#'
          | otherwise               = '.'

    splitlines [] = []
    splitlines l  = (take 40 l) : splitlines (drop 40 l)
