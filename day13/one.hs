import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

data Packet = Packets [Packet] | PNumber Integer
  deriving (Eq, Show)

lessThan (Packets (ps1:ps1s)) (Packets (ps2:ps2s))
  | lessThan ps1 ps2 == 0 = lessThan (Packets ps1s) (Packets ps2s)
  | otherwise = lessThan ps1 ps2
lessThan (Packets []) (Packets (ps2:ps2s)) = -1
lessThan (Packets (ps1:ps1s)) (Packets []) = 1
lessThan (Packets []) (Packets []) = 0
lessThan ps@(Packets _) pn@(PNumber n) = lessThan ps (Packets [pn])
lessThan pn@(PNumber _) ps@(Packets _) = lessThan (Packets [pn]) ps
lessThan (PNumber n1) (PNumber n2)
  | n1 < n2  = -1
  | n1 == n2 = 0
  | n1 > n2  = 1

readD :: Parser [(Packet,Packet)]
readD = readCase `sepEndBy` (many1 newline)
  where
    readCase = do
      p1 <- readPacket
      newline
      p2 <- readPacket
      return (p1,p2)

    readPacket = choice [readPackets, readNumber]

    readPackets = do
      char '['
      ps <- readPacket `sepBy` char ','
      char ']'
      return (Packets ps)

    readNumber = do
      n <- number
      return (PNumber n)
      

solve inp = unlines [
  unlines . map show $ inp
  , unlines . map show $ [lessThan p1 p2 | (p1,p2) <- inp]
  , show $ sum indices
  ]
  where
    orders = [lessThan p1 p2 | (p1,p2) <- inp]
    indices = map fst . filter ((<0) . snd) $ zip [1..] orders
