import Text.Parsec
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

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
        solve' known unknown
          | M.null unknown = known
          | otherwise      = solve' known' unknown'
          where
            reduced = M.map tryReduce unknown

            known' = M.union known $ M.filter isKnown reduced
            unknown' = M.filter (not . isKnown) reduced

            calc '+' (Left n1) (Left n2) = n1 + n2
            calc '*' (Left n1) (Left n2) = n1 * n2
            calc '/' (Left n1) (Left n2) = div n1 n2
            calc '-' (Left n1) (Left n2) = n1 - n2

            tryReduce (Left n) = Left n
            tryReduce (Right (op, a, b))
              | a `M.member` known && b `M.member` known = Left $ calc op (known M.! a) (known M.! b)
              | otherwise = Right (op, a, b)

        isKnown (Left n) = True
        isKnown (Right _) = False
