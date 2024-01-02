import Text.Parsec
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
  , show $ compactedChart
  , show $ length paths
  , show $ maxFlow
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

    maxM = 30

    findPaths from minute visited
      | minute >= maxM = [[]]
      | length visited == length interesting = [[(from, minute+1)]]
      | otherwise = nextPaths
      where
        (flow, others) = compactedChart M.! from
        nextPaths = [(from,minute):rest |
                     (other, dist) <- others,
                     rest <- if (elem other visited) then [[]] else findPaths other (minute+1+dist) (from:visited)]

    paths = findPaths "AA" 0 []

    getFlow path = sum $ map getFlow' path
      where
        getFlow' (valve, minute)
          | minute >= maxM = 0
          | otherwise      = (maxM-minute)*flow
          where
            (flow, _) = compactedChart M.! valve

    maxFlow = maximum . map getFlow $ paths
