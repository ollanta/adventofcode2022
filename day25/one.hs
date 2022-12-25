import Text.Parsec
import Data.List
import Data.Char
import Parsing

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = (many1 (oneOf "=-012")) `sepEndBy` newline


translator = zip "=-0123" [-2..2]
readElf c = head [k | (e,k) <- translator, c == e]
writeElf n = head [c | (c,k) <- translator, n == k]


solve inp = unlines [
  unlines $ inp
  , unlines . map show $ translated
  , unlines . map revNumb $ translated
  , revNumb (sum translated)
  ]
  where
    translated = map toNumb inp

    toNumb l = sum . readElfCode $ reverse l
      where
        readElfCode rcode = zipWith (*) (iterate (*5) 1) (map readElf rcode)

    revNumb k = reverse $ findN 1 k
      where
        findN _ 0 = []
        findN p k = writeElf r' : findN p' (k-p*r')
          where
            p' = p*5
            r = k `rem` p'
            r' = fixRem (r `div` p)

            fixRem k
              | k >= 3    = k-5
              | otherwise = k
