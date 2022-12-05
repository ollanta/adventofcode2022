import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = interact ( solve . readD )

data Command = Cd String | Ls [File]
  deriving (Show, Eq)

data File = Dir String | File String Integer
  deriving (Show, Eq)


readD s = readcoms $ lines s
  where
    readcoms [] = []
    readcoms (s:ss)
      | (take 4 s) == "$ cd" = Cd (drop 5 s) : readcoms ss
      | (take 4 s) == "$ ls" = Ls output : readcoms ss'
      where
        output = map readfile . takeWhile ((/='$') . head) $ ss
        ss' = drop (length output) ss

    readfile s
      | (take 3 s) == "dir" = Dir (drop 4 s)
      | otherwise = File name size
      where
        size = read . takeWhile (/=' ') $ s
        name = drop 1 . dropWhile (/=' ') $ s

solve :: [Command] -> String
solve inp = unlines $ [
  show dirSizes,
  show ans,
  show ans2]
  where
    initStruct = M.empty
    fileStruct = toStruct initStruct [] inp
    
    toStruct :: M.HashMap [String] [File] -> [String] -> [Command] -> M.HashMap [String] [File]
    toStruct m cd (Cd s:cs)
      | s == ".." = toStruct m (tail cd) cs
      | s == "/" = toStruct m [] cs
      | otherwise = toStruct m (s:cd) cs
    toStruct m cd (Ls files:cs) = toStruct (M.insert cd files m) cd cs
    toStruct m _ [] = m

    dirSizes = M.mapWithKey getSize fileStruct
      where
        getSize dir files = sum . map fileSize $ files
          where
            fileSize (File _ size) = size
            fileSize (Dir d) = getSize (d:dir) (fileStruct M.! (d:dir))

    ans = sum . filter (<=100000) . M.elems $ dirSizes

    freeSpace = 70000000 - dirSizes M.! []
    spaceNeeded = 30000000

    ans2 = sort . filter (\s -> freeSpace + s >= spaceNeeded) . M.elems $ dirSizes
