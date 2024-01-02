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
  show interesting
  , show paths
  , show $ findFlow initHeap
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

    paths = M.fromList [ (valve, [ (otherValve, shortestPath valve otherValve) | (otherValve, _) <- interesting, valve /= otherValve]) |
                         (valve, flow) <- interesting]
    flows = M.fromList interesting

    initHeap :: H.MaxPrioHeap Integer (Integer, (Integer, String), (Integer, String), [String])
    initHeap = H.fromList [(maxM * calcFlow [], (0, (0, "AA"), (0, "AA"), ["AA"]))]

    maxM = 26
    minutesLeft m = max (maxM-m) 0

    calcFlow openValves = sum [ flows M.! valve | valve <- openValves]

    closedFlow = calcFlow . map fst $ interesting

    findFlow :: H.MaxPrioHeap Integer (Integer, (Integer, String), (Integer, String), [String]) -> (Integer, (Integer, (Integer, String), (Integer, String), [String]))
    findFlow prioh
      | length openValves == length interesting = (potential, state)
      | otherwise            = findFlow prioh''
      where
        Just ((potential, state), prioh') = H.view prioh
        (totalFlow, actor1, actor2, openValves) = state
        nextActor = min actor1 actor2
        lastActor = max actor1 actor2

        prioh'' = H.union prioh' $ H.fromList [(prio, (flow, actor', lastActor, open)) | (prio, (flow, actor', open)) <- moveFrom nextActor]

        potentialFlow actors open = sum [ flows M.! valve * minutesLeft minute | (valve, minute) <- M.toList nextMoves ]
          where
            nextMoves = M.fromListWith min [(valve, minute+dist+1) |
                                            (minute, pos) <- actors,
                                            (valve, dist) <- paths M.! pos,
                                            not (valve `elem` open)]

        moveFrom (minutes, pos) = goToOther
          where
            leftToOpen = [(next, k) | (next, k) <- paths M.! pos, not (elem next openValves)]

            goToOther = [ (totalFlow' + potentialFlow [(minutes', next), lastActor] openValves',
                           (totalFlow', (minutes', next), openValves'))
                        | (next, k) <- leftToOpen,
                          let minutes' = minutes+k+1,
                          let flowAtNext = flows M.! next,
                          let totalFlow' = totalFlow + flowAtNext * minutesLeft minutes',
                          let openValves' = next:openValves]

