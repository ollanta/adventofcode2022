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
  | lessThan ps1 ps2 == EQ = lessThan (Packets ps1s) (Packets ps2s)
  | otherwise = lessThan ps1 ps2
lessThan (Packets []) (Packets (ps2:ps2s)) = LT
lessThan (Packets (ps1:ps1s)) (Packets []) = GT
lessThan (Packets []) (Packets []) = EQ
lessThan ps@(Packets _) pn@(PNumber n) = lessThan ps (Packets [pn])
lessThan pn@(PNumber _) ps@(Packets _) = lessThan (Packets [pn]) ps
lessThan (PNumber n1) (PNumber n2)
  | n1 < n2  = LT
  | n1 == n2 = EQ
  | n1 > n2  = GT

readD :: Parser [Packet]
readD = readPacket `sepEndBy` (many1 newline)
  where
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
  show $ product indices
  ]
  where
    dividers = [Packets [Packets [PNumber 2]],Packets [Packets [PNumber 6]]]
    inp' = inp ++ dividers

    reordered = sortBy lessThan inp'

    indices = map fst . filter (isdivider . snd) $ zip [1..] reordered

    isdivider d = d `elem` dividers
