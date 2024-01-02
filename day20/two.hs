import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [Integer]
readD = readLine `sepEndBy` newline
  where
    readLine = mnumber


solve inp = unlines [
  show $ inp
  , show $ grovec
  , show $ sum grovec
  --, show . sum . map (\(a,b) -> a*b) $ anss
  ]
  where
    inlength = toInteger $ length inp

    key = 811589153

    mixed = mix $ map (key*) inp

    grovec = [mixedm M.! getI n | n <- [1000,2000,3000]]
      where
        zero = head [ i | (i,n) <- mixed, n == 0]

        getI n = (zero + n) `mod` inlength

        mixedm = M.fromList mixed

    mix nums = mix' 10 [(i, i, n, False) | (i,n) <- zip [0..] nums]
      where
        mix' k m
          | k == 0        = curr
          | null notMoved = mix' (k-1) [(i, o, n, False) | (i, o, n, b) <- m]
          | otherwise     = mix' k m'
          where
            curr = [ (i,n) | (i, order, n, b) <- sort m]

            notMoved = [(order, i, n) |
                        (i, order, n, moved) <- m,
                        not moved]
            (mo, mi, mn) = minimum notMoved

            newi = (mi+mn) `mod` (inlength-1)

            m' = map updateI m
            
            updateI (i, o, n, b)
              | i == mi = (newi, o, n, True)
              | i < mi && i < newi = (i, o, n, b)
              | i < mi && i >= newi = (i+1, o, n, b)
              | i > mi && i <= newi = (i-1, o, n, b)
              | i > mi && i > newi  = (i, o, n, b)
