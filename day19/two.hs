import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart3d
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer,[(String, M.HashMap String Integer)])]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      string "Blueprint "
      n <- number
      string ":"
      rs <- readRobot `sepEndBy` string "."
      return (n,rs)

    readRobot = do
      string " Each "
      prod <- many1 alphaNum
      string " robot costs "
      cost <- readCost `sepBy` string " and "
      return (prod, M.fromList cost)
      
    readCost = do
      n <- number
      string " "
      t <- many alphaNum
      return (t,-n)


solve inp = unlines [
  show $ inp
  --, unlines . map (unlines . map show) $ map (search initRobots initStash) inp
  --, unlines . map (unlines . map (unlines . map show)) $ map (search initRobots initStash) inp
  --, unlines . map (unlines . map show . take 100) $ qualities
  , unlines . map show $ qualities
  , show . product . map snd $ qualities
  ]
  where
    inp' = take 3 inp
    qualities = map (search initRobots initStash) inp'

    initRobots :: M.HashMap String Integer
    initRobots = M.fromList [("ore", 1)]
    initStash :: M.HashMap String Integer
    initStash = M.fromList []

    maxM = 32

    search irobots istash (bi,blueprints) = search' initHeap
      where
        cheapprint = M.fromList [(robot, M.insert "ore" 0 price) | (robot, price) <- blueprints]

        initHeap :: H.MaxPrioHeap Integer (Integer, (M.HashMap String Integer, M.HashMap String Integer))
        initHeap = H.singleton (1000, (1, (irobots, istash)))

        search' :: H.MaxPrioHeap Integer (Integer, (M.HashMap String Integer, M.HashMap String Integer)) -> (Integer, Integer) -- [(Integer, (Integer, (M.HashMap String Integer, M.HashMap String Integer)))]
        search' prioh
          -- | minute == maxM = [(potential, (minute, state))]
          -- | otherwise = (potential, (minute, state)) : search' prioh''
          | minute == maxM = (bi, getAmountOf "geode" . updateStash $ state)
          | otherwise = search' prioh''
          where
            Just ((potential, mstate), prioh') = H.view prioh
            (minute, state@(robots, stash)) = mstate

            getAmountOf ore stash = M.lookupDefault 0 ore stash
            updateStash (robots, stash) = M.unionWith (+) robots stash

            buyRobots (robots,stash) = (robots,stash):[(M.insertWith (+) robot 1 robots, deducted) |
                                        (robot,price) <- blueprints,
                                        let deducted = M.unionWith (+) stash price,
                                            all (>=0) (M.elems deducted)]

            newstates = [(minute+1, (robots', M.unionWith (+) robots stash')) |
                         (robots', stash') <- buyRobots state]

            withpotentials = [(getpotential mstate, mstate) | mstate <- newstates]
            prioh'' =  H.union prioh' (H.fromList withpotentials)
            getpotential (m, (robots, stash)) = simulate m robots stash
                                                

            simulate :: Integer -> M.HashMap String Integer -> M.HashMap String Integer -> Integer
            simulate mn r s
              | mn == maxM  = getAmountOf "geode" $ updateStash (r, s)
              | otherwise   = simulate (mn+1) r' (M.unionWith (+) r s')
              where
                builds = [(M.insertWith (+) builder 1 r, deducted) |
                          builder <- ["geode", "obsidian", "clay", "ore"],
                          let price = cheapprint M.! builder,
                          let deducted = M.unionWith (+) s price,
                              all (>=0) (M.elems deducted)]
                (r',s') = head builds
