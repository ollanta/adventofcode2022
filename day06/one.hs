import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = interact solve


solve :: String -> String
solve inp = show $ solve' 0 inp
  where
    solve' n s
      | (length . nub . take 4 $ s) == 4 = n + 4
      | otherwise = solve' (n+1) (tail s)
