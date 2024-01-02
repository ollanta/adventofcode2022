import Text.Parsec
import Data.List
import Data.Char
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer, M.HashMap String (M.HashMap String Integer))]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      string "Blueprint "
      n <- number
      string ":"
      rs <- readRobot `sepEndBy` string "."
      return (n, M.fromList rs)

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
  show $ inp'
  --, unlines . map (unlines . map (unlines . map show)) $ map (search initRobots initStash) inp'
  , unlines . map show $ qualities
  , show . sum . map (\(a,b) -> a*b) $ qualities
  ]
  where
    qualities = map (search initRobots initStash) inp'

    initRobots :: [Integer]
    initRobots = [1,0,0,0]
    initStash :: [Integer]
    initStash = [0,0,0,0]

    inp' = map listify inp
      where
        listify (bi,robots) = (bi, [[M.lookupDefault 0 ore (robots M.! builder) | ore <- ores] | builder <- ores])
        ores = ["ore", "clay", "obsidian", "geode"]

    search robots stash (bi,blueprints) = search' [(robots, stash)] 1
      where
        search' states minute
          | minute == 24 = (bi, maximum . map (last . snd . updateStash) $ states)
          | otherwise = search' states'' (minute+1)
          where
            updateStash (robots, stash) = (robots, zipWith (+) robots stash)

            buildRobot 0 [a,b,c,d] = [a+1,b,c,d]
            buildRobot 1 [a,b,c,d] = [a,b+1,c,d]
            buildRobot 2 [a,b,c,d] = [a,b,c+1,d]
            buildRobot 3 [a,b,c,d] = [a,b,c,d+1]

            buyRobots (robots,stash) = (robots,stash):[(buildRobot i robots, deducted) |
                                        (i, price) <- zip [0..] blueprints,
                                        let deducted = zipWith (+) stash price,
                                            all (>=0) deducted]

            states' = [(robots', zipWith (+) robots stash') |
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
                     strictlyBetter s1 s2 = and $ zipWith (>=) s1 s2

                  
