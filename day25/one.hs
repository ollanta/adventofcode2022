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

    revNumb k = reverse $ findN k
      where
        findN 0 = []
        findN k = writeElf r : findN q
          where
            (q, r) = fixRem $ k `quotRem` 5

            fixRem (q, r)
              | r >= 3    = (q+1, r-5)
              | otherwise = (q, r)
