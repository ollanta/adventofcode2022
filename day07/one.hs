import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

data Command = Cd String | Ls [File]
  deriving (Show, Eq)

data File = Dir String | File String Integer
  deriving (Show, Eq)

readD :: Parser [Command]
readD = readCommand `endBy` newline
  where
    readCommand = do
      string "$ "
      c <- choice [readCD, readLS]
      return c

    readCD = do
      string "cd "
      dir <- choice [many1 alphaNum, string "..", string "/"]
      return (Cd dir)

    readLS = do
      string "ls"
      newline
      files <- readResult `sepBy` (try specialNewline)
      return $ Ls files

    specialNewline = do
      newline
      notFollowedBy (char '$')

    readResult = do
      r <- try (choice [readDir, readFile])
      return $ r

    readDir = do
      string "dir "
      dir <- many1 alphaNum
      return (Dir dir)

    readFile = do
      n <- number
      string " "
      name <- many1 (choice [alphaNum, char '.'])
      return (File name n)



solve :: [Command] -> String
solve inp = show inp
