import Text.Parsec
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

    mix nums = (!!10) . iterate (mix' . toTriple) $ zip [0..] nums
      where
        toTriple = map (\(a, b) -> (a, b, False))

        mix' m
          | null notMoved = curr
          | otherwise     = mix' m'
          where
            curr = [ (i,n) | (i, n, b) <- m]

            notMoved = [(i, n) |
                        (i, n, moved) <- m,
                        not moved]
            (mi, mn) = head notMoved

            newi = (mi+mn) `mod` (inlength-1)

            m' = map updateI m
            
            updateI (i, n, b)
              | i == mi = (newi, n, True)
              | i < mi && i < newi = (i, n, b)
              | i < mi && i >= newi = (i+1, n, b)
              | i > mi && i <= newi = (i-1, n, b)
              | i > mi && i > newi  = (i, n, b)
