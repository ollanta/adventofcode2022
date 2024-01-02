import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart3d
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(String, Either Integer (Char,String,String))]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      name <- many1 alphaNum
      string ": "
      op <- choice [Left <$> number, Right <$> readOp]
      return (name, op)

    readOp = do
      n1 <- many1 alphaNum
      spaces
      op <- oneOf "+-*/"
      spaces
      n2 <- many1 alphaNum
      return (op, n1, n2)


solve inp = unlines [
  show $ inp,
  show $ solve inp M.! "root"
  ]
  where
    inlength = toInteger $ length inp
    

    solve :: [(String, Either Integer (Char,String,String))] -> M.HashMap String (Either Integer (Char,String,String))
    solve i = solve' M.empty (M.fromList i)
      where
        solve' known rest
          | M.null rest = known
          | otherwise    = solve' known' rest''
          where
            rest' = M.map tr rest

            nowknown = M.filter isKnown rest'

            known' = M.union known nowknown
            rest'' = M.difference rest' nowknown

            calc '+' (Left n1) (Left n2) = n1 + n2
            calc '*' (Left n1) (Left n2) = n1 * n2
            calc '/' (Left n1) (Left n2) = div n1 n2
            calc '-' (Left n1) (Left n2) = n1 - n2

            tr (Left n) = Left n
            tr (Right (op, a, b))
              | a `M.member` known && b `M.member` known = Left $ calc op (known M.! a) (known M.! b)
              | otherwise = Right (op, a, b)

        isKnown (Left n) = True
        isKnown (Right _) = False
