module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils (filterP, mapAt, readInt, tok)

import Debug.Trace (trace)

----------------
-- Some sugar --
----------------

-- The amounts of the four robots we have.
type RobotCounts = [Int]
-- The amounts of the four ressources.
type Ressources  = [Int]
-- A state is defined by the amounts of ressources and the
-- counts of robots we own. Note that a tuple of tuples
-- gets Ord for free, so if s1 <= s2, then s1 is clearly
-- inferior compared to s2.
type State       = (Ressources, RobotCounts)

-- The costs of the four ressources we need to produce a given robot.
type Costs       = [Int]
-- A blueprint provides the costs for the four kinds of robots we can build.
type Blueprint   = [Costs]

------------------
-- Some helpers --
------------------

-- Given a blueprint, check whether a state is wasteful, i.e.
-- it the state produces more of any of the first three
-- ressources than can possibly be spent on anything.
-- (Production = number of robots.)
isWasteful :: Blueprint -> State -> Bool
isWasteful blueprint (_, production) = or (zipWith (>) production maxSpend)
  where
    maxSpend = map (\i -> maximum (map (!!i) blueprint)) [0..2]

-- Given a blueprint, check which robots can be built in the current state.
boolPoss :: Blueprint -> State -> [Bool]
boolPoss blueprint (ressources, _) = map checkRobot blueprint
  where
    checkRobot costs = and (zipWith (<=) costs ressources)

-- Check if state 1 is equal to or worse than state 2, i.e.
-- if the numbers of *all* ressources and robots are <=.
lesseq :: State -> State -> Bool
lesseq (ress1, robos1) (ress2, robos2) = and
                                       . zipWith (<=) (ress1 ++ robos1)
                                       $ (ress2 ++ robos2)

-------------   
-- Parsing --
-------------

-- Parse a single input line to a 4-list, containing the costs of the
-- ore, clay, obsidian, geode robots.
parseLine :: String -> Blueprint
parseLine line = [oreRobot, clayRobot, obsidianRobot, geodeRobot]
  where
    oreRobot      = [numbers !! 0, 0,            0,            0]
    clayRobot     = [numbers !! 1, 0,            0,            0]
    obsidianRobot = [numbers !! 2, numbers !! 3, 0,            0]
    geodeRobot    = [numbers !! 4, 0,            numbers !! 5, 0]
    numbers       = map readInt
                  . tok " " 
                  . filter (`elem` "1234567890 ")
                  . dropWhile (/=':')
                  $ line

--------------------------------
-- The actual programme logic --
--------------------------------

-- Given a current state and a blueprint, find all possible next states.
-- Wasteful states are immediately discarded.
-- If we can buy a geode robot, we do so.
-- If we can build all robots, doNothing is not an option.
nextStates :: Blueprint -> State -> [State]
nextStates blueprint state@(ressources, robots)
    | isWasteful blueprint state      = []
    | last (boolPoss blueprint state) = [last buildSteps]
    | and (boolPoss blueprint state)  = buildSteps
    | otherwise                       = doNothingStep : buildSteps
      where
        doNothingStep    = mine (ressources, robots)
        buildSteps       = map (\ robotIdx -> build robotIdx . mine $ state)
                         $ filterP (boolPoss blueprint state) [0..3]

        mine    (re, rb) = (zipWith (+) re rb, rb)
        build i (re, rb) = (re', rb')
          where
            re' = zipWith (-) re (blueprint !! i)
            rb' = mapAt i (+1) robots

-- Count the number of geodes a blueprint can produce at maximum
geodes :: Int -> Blueprint -> Int
geodes tmax bp = go [([0,0,0,0],[1,0,0,0])] tmax
  where
    go :: [State] -> Int -> Int
    go states 0    = maximum $ map ((!!3) . fst) states
    go states time = trace (show time ++ ": " ++ show (length (states))) go states' (time-1)
      where
        states'   = eliminate $ concat $ map (nextStates bp) $ states

-- Eliminate all states that are lesseq than at least one other state in the list
eliminate :: [State] -> [State]
eliminate states = go [] states []
  where
    go _ [] accum = accum
    go done queue@(q:qs) accum
      |or (map (\q2 -> lesseq q q2) (done++qs)) = go (q:done) qs accum
      |otherwise                                = go (q:done) qs (q:accum)

main :: IO ()
main = do
    blueprints <- map parseLine . lines <$> readFile "input.txt"

    -- Part 1
    print . sum
          . zipWith (*) [1..]
          . map (geodes 24)
          $ blueprints

    -- Part 2
    print . foldl1 (*)
          . map (geodes 32)
          $ take 3 blueprints

    print $ "Done."
