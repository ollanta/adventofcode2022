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
  show (getDirSizes fileStruct),
  show ans,
  show ans2]
  where
    initStruct = M.empty
    fileStruct = toStruct initStruct [] inp
    
    toStruct :: M.HashMap String [File] -> [String] -> [Command] -> M.HashMap String [File]
    toStruct m cd (Cd s:cs)
      | s == ".." = toStruct m (take (length cd - 1) cd) cs
      | s == "/" = toStruct m [] cs
      | otherwise = toStruct m (cd ++ [s]) cs
    toStruct m cd (Ls files: cs) = toStruct m' cd cs
      where
        m' = M.insert (joindir cd) files m
    toStruct m _ [] = m

    joindir [] = "/"
    joindir (s:ss) = "/" ++ s ++ joindir ss

    getDirSizes m = map (\(d, _) -> (d, getSize d)) $ M.toList m
      where
        getSize dir = sum . map fileSize $ m M.! dir
          where
            fileSize (File _ size) = size
            fileSize (Dir d) = getSize (dir ++ d ++ "/")


    ans = sum . filter (<=100000) . map snd $ getDirSizes fileStruct

    freeSpace = 70000000 - ((M.fromList (getDirSizes fileStruct)) M.! "/")
    ans2 = sort . filter (>=30000000-freeSpace) . map snd $ (getDirSizes fileStruct)
