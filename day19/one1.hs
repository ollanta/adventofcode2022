import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M
import Chart3d

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer,[(String,[(String,Integer)])])]
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
      return (prod, cost)
      
    readCost = do
      n <- number
      string " "
      t <- many alphaNum
      return (t,-n)


{-
Same as one but map-based
-}


solve inp = unlines [
  show $ inp
  --, unlines . map (unlines . map show) $ map (search initRobots initStash) inp
  --, unlines . map (unlines . map (unlines . map show)) $ map (search initRobots initStash) inp
  , unlines . map show $ qualities
  , show . sum . map (\(a,b) -> a*b) $ qualities
  ]
  where
    qualities = map (search initRobots initStash) inp

    initRobots :: M.HashMap String Integer
    initRobots = M.fromList [("ore", 1)]
    initStash :: M.HashMap String Integer
    initStash = M.fromList []

    search robots stash (bi,blueprints) = search' [(robots, stash)] 1
      where
        search' states minute
          | minute == 24 = (bi, maximum . map (getAmountOf "geode" . updateStash) $ states)
          | otherwise = search' states'' (minute+1)
          where
            getAmountOf ore stash = M.lookupDefault 0 ore stash
            updateStash (robots, stash) = M.unionWith (+) robots stash

            buyRobots (robots,stash) = (robots,stash):[(M.insertWith (+) robot 1 robots, deducted) |
                                        (robot,price) <- blueprints,
                                        let deducted = M.unionWith (+) stash (M.fromList price),
                                            all (>=0) (M.elems deducted)]

            states' = [(robots', M.unionWith (+) robots stash') |
                       (robots, stash) <- states,
                       (robots', stash') <- buyRobots (robots, stash)]

            states'' = optimal states'

            optimal sts = [(r,s) |
                           (r,ss) <- M.toList optByRobot,
                           s <- ss]
              where
                optByStash = M.fromListWith takeOptimal [(stash,[robots]) | (robots, stash) <- sts]
                optByRobot = M.fromListWith takeOptimal [(robots, [stash]) | (stash, robotses) <- M.toList optByStash, robots <- robotses]


                takeOptimal [stash] [] = [stash]
                takeOptimal [stash] (o:opts)
                  | o `strictlyBetter` stash = o:opts
                  | stash `strictlyBetter` o = takeOptimal [stash] opts
                  | otherwise                = o:takeOptimal [stash] opts
                  where
                     strictlyBetter s1 s2 = and [ M.lookupDefault 0 s s1 >= n | (s,n) <- M.toList s2]
