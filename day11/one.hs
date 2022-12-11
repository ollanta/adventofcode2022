import Text.Parsec
import Data.List
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer, [Integer], Char, Either Integer String, Integer, Integer, Integer)]
readD = readMonkey `endBy` (many newline)
  where
    readMonkey = do
      string "Monkey "
      n <- number
      string ":"
      many space
      string "Starting items: "
      items <- (number `sepBy` string ", ")
      many space
      string "Operation: new = old "
      op <- oneOf "+*"
      space
      other <- readOther
      many space
      string "Test: divisible by "
      d <- number
      many space
      string "If true: throw to monkey "
      true <- number
      many space
      string "If false: throw to monkey "
      false <- number
      return (n, items, op, other, d, true, false)

    readOther = choice [readN, readOld]
      where
        readN = do
          n <- number
          return (Left n)
        readOld = do
          o <- string "old"
          return (Right o)
      

solve inp = unlines [
  show inp,
  unlines . map (show. fst) $ maps,
  unlines . map (show. snd) $ maps,
  show . product . take 2 . reverse . sort . M.elems . fst . last $ maps
  ]
  where
    monkeyMap = M.fromList [(n, items) | (n, items, _, _, _, _, _) <- inp]

    inspectMap = M.fromList [(n, 0) | (n, _, _, _, _, _, _) <- inp]

    runMonkeys c m [] = [(c,m)]
    runMonkeys c m ((n,_,op,other,d,true,false):is) = (c,m) : runMonkeys c' m'' is
      where
        items = reverse $ m M.! n

        c' = M.adjust (+ length items) n c

        m' = M.insert n [] m

        updates = [newMI i | i <- items]

        newMI i = (m', i'')
          where
            i' = newI i op other
            newI x '+' (Left y) = x+y
            newI x '*' (Left y) = x*y
            newI x '+' (Right _) = x+x
            newI x '*' (Right _) = x*x

            i'' = i' `div` 3

            m' = newM  i'' d true false

            newM x divisor true false
              | x `mod` divisor == 0 = true
              | otherwise            = false

        m'' = foldl throwTo m' updates
          where
            throwTo mm (m', i') = M.adjust (i':) m' mm

    maps = runMonkeys inspectMap monkeyMap (concat . replicate 20 $ inp)

    
              
                 
