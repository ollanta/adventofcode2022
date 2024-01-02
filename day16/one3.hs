import Text.Parsec
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Data.Heap as H

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
  , show $ compactedChart
  , show $ findFlow M.empty initHeap
  ]
  where
    chart = M.fromList inp

    interesting = [(valve, flow) | (valve, (flow, _)) <- inp, flow > 0 || valve == "AA"]

    shortestPath :: String -> String -> Integer
    shortestPath start end = search M.empty [start]
      where
        search visited poss
          | end `elem` poss = 0
          | otherwise       = 1 + search visited' poss'
          where
            visited' = M.union visited (M.fromList $ zip poss (repeat True))
            poss' = [ pos | pos <- concatMap findNeighs poss, not (M.member pos visited')]

        findNeighs pos = snd $ chart M.! pos

    compactedChart = M.fromList [ (valve, (flow, [ (otherValve, shortestPath valve otherValve) | (otherValve, _) <- interesting, valve /= otherValve])) |
                          (valve, flow) <- interesting]


    initHeap :: H.MaxPrioHeap Integer (Integer, Integer, String, [String])
    initHeap = H.fromList [(maxM * calcFlow [], (0, 0, "AA", []))]

    maxM = 31
    minutesLeft m = max (maxM-m) 0

    calcFlow openValves = sum [ flow | valve <- openValves, let (flow, _) = compactedChart M.! valve]

    findFlow :: M.HashMap (String, [String]) Bool -> H.MaxPrioHeap Integer (Integer, Integer, String, [String]) -> Integer -- [(Integer, (Integer, String, [String]))]
    findFlow visited prioh
      | null leftToOpen = totalFlow'
      | M.member (pos, openValves) visited = findFlow visited prioh'
      | otherwise                          = findFlow visited' (H.union prioh' $ H.fromList goToOther)
      where
        Just ((potential, state), prioh') = H.view prioh
        (totalFlow, minutes, pos, openValves) = state

        visited' = M.insert (pos, openValves) True visited

        (flow, neighs) = compactedChart M.! pos

        leftToOpen = [(next, k) | (next, k) <- neighs, not (elem next openValves)]
        closedFlow = calcFlow . map fst $ leftToOpen

        minutes' = minutes+1
        totalFlow' = totalFlow + minutesLeft minutes' * flow

        goToOther = [ (totalFlow' + (closedFlow * minutesLeft (minutes' + k + 1)),
                      (totalFlow', minutes'+k, next, pos:openValves))
                    | (next, k) <- leftToOpen ]
