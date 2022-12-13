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

instance Ord Packet where
 compare (Packets ps1) (Packets ps2) = compare ps1 ps2
 compare (PNumber n1)  (PNumber n2)  = compare n1 n2
 compare ps@(Packets _) pn           = compare ps (Packets [pn])
 compare pn           ps@(Packets _) = compare (Packets [pn]) ps

readD :: Parser [Packet]
readD = readPacket `sepEndBy` (many1 newline)
  where
    readPacket = choice [
      between (char '[') (char ']') (Packets <$> readPacket `sepBy` char ',')
      , PNumber <$> number
      ]
      

solve inp = unlines [
  show $ product indices
  ]
  where
    Right dividers = parse readD "" (unlines ["[[2]]","[[6]]"])
    inp' = inp ++ dividers

    indices = [ind | (ind, p) <- zip [1..] (sort inp'), p `elem` dividers]
