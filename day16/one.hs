import Text.Parsec
import Data.List
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(String, (Integer, [String]))]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      string "Valve "
      valve <- readValve
      string " has flow rate="
      flow <- number
      string "; tunnel"
      optional (string "s")
      string " lead"
      optional (string "s")
      string " to valve"
      optional (string "s")
      string " "
      neighs <- readValve `sepBy` string ", "
      return (valve, (flow, neighs))

    readValve = many1 alphaNum


solve inp = unlines [
  show $ inp
  , show $ move 29 M.empty 0 [(("AA",[]),0)]
  ]
  where
    chart = M.fromList inp

    move :: Integer -> M.HashMap (String,[String]) Integer -> Integer -> [((String,[String]),Integer)] -> Integer
    move maxT visited minute states
      | maxT == minute = maximum [ total | (_, total) <- states]
    move maxT visited minute states = move maxT visited' (minute+1) uniqueStates
      where
        visited' = M.union (M.fromList $ states) visited

        states' = concatMap genNewStates states

        uniqueStates = M.toList . M.fromListWith max $ states'

        genNewStates ((pos, openValves), total) = newStates
          where
            (newFlow, neighs) = chart M.! pos

            openThisValve = if (pos `elem` openValves || newFlow == 0) then [] else [((pos, sort (pos:openValves)), total+(maxT-minute)*newFlow)]
            moves = [((neigh, openValves), total) | neigh <- neighs]

            newStates = openThisValve ++ moves
