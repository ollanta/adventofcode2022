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
  show $ inp
  ,show $ solved
  ,show $ expand left
  ,show $ expand right
  ,show $ contract (allsk M.! left) (allsk M.! right)
  ]
  where
    inlength = toInteger $ length inp
    
    (solved, known) = solve inp
    allsk = M.union solved known

    Right (_, left, right) = solved M.! "root"

    expand str
      | M.member str known = show val
      | str == "humn"      = "humn"
      | M.member str solved = "(" ++ expand a ++ ")" ++ (op:[]) ++ "(" ++ expand b ++ ")"
      | otherwise          = "?"++str++"?"
      where
        Left val = known M.! str
        Right (op,a,b) = solved M.! str

    contract (Right (op,a,b)) (Left k)
      | a == b && a == "humn" = k
      | otherwise = contract r' (Left k')
      where
        va = allsk M.! a
        vb = allsk M.! b

        (r', k') = reduce op va vb

        reduce '+' (Left v) b = (b, k-v)
        reduce '+' a (Left v) = (a, k-v)
        reduce '-' (Left v) b = (b, v-k)
        reduce '-' a (Left v) = (a, k+v)
        reduce '*' (Left v) b = (b, div k v)
        reduce '*' a (Left v) = (a, div k v)
        reduce '/' (Left v) b = (b, div v k)
        reduce '/' a (Left v) = (a, k * v)

    solve :: [(String, Either Integer (Char,String,String))] -> (M.HashMap String (Either Integer (Char,String,String)), M.HashMap String (Either Integer (Char,String,String)))
    solve i = solve' M.empty minit''
      where
        minit = M.fromList i
        minit' = M.insert "humn" (Right ('=',"humn","humn")) minit
        minit'' = M.adjust (\(Right (op,a,b)) -> Right ('=',a,b)) "root" minit'

        solve' known unknown
          | known == known' = (reduced, known)
          | otherwise       = solve' known' unknown'
          where
            reduced = M.map tryReduce unknown

            known' = M.union known $ M.filter isKnown reduced
            unknown' = M.filter (not . isKnown) reduced

            calc '+' (Left n1) (Left n2) = n1 + n2
            calc '*' (Left n1) (Left n2) = n1 * n2
            calc '/' (Left n1) (Left n2) = div n1 n2
            calc '-' (Left n1) (Left n2) = n1 - n2
            calc '=' (Left n1) (Left n2) = n1 - n2

            tryReduce (Left n) = Left n
            tryReduce (Right (op, a, b))
              | op == '=' = Right (op, a, b)
              | a `M.member` known && b `M.member` known = Left $ calc op (known M.! a) (known M.! b)
              | otherwise = Right (op, a, b)

    isKnown (Left n) = True
    isKnown (Right _) = False
