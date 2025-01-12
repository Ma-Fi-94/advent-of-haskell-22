module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils (filterP, mapAt, readInt, tok)

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
worse :: State -> State -> Bool
worse (ress1, robos1) (ress2, robos2) = and
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


-- A merge function for two sets of State-s. The first Set is assumed
-- to not contain any states which are worse than any other one.
-- All states in the second Set are then checked:
-- If the state is worse than any state in the first Set, it is not added.
-- If it is better than any state in the first Set, it is added, and all
-- worse states from the first Set are dropped.
setMerge :: Set State -> Set State -> Set State
setMerge set1 set2 = go set1 (Set.elems set2)
  where
    go s1 []     = s1
    go s1 (s:ss) = go s1' ss
      where
        s1' = if   (s `worse`) `any` s1
              then s1
              else Set.insert s . Set.filter (not . (`worse` s)) $ s1

-- And the same for a list of Sets
setsMerge :: [Set State] -> Set State
setsMerge sets = foldl setMerge Set.empty sets

-- Some additional pruning. Needs number of time steps remaining.
-- For every state, we calculate a lower bound of the number of geodes
-- produced at the end of the game. Then, we remove all states with a
-- lower bound 50% and more below the maximum.
-- We only start pruning if we have <10 time steps remaining.
prune :: Int -> Set State -> Set State
prune tRem states = if   tRem < 10
                    then Set.filter ((>=cutoff) . bound) states
                    else states
  where
    cutoff = (`div` 2) . maximum . map bound . Set.elems $ states
    bound  = (\ (ressources, robots) -> (ressources !! 3) + tRem * (robots !! 3))

-- Count the number of geodes a blueprint can produce at maximum
nbGeodes :: Int -> Blueprint -> Int
nbGeodes tmax blueprint = go (Set.singleton ([0,0,0,0],[1,0,0,0])) tmax
  where
    go :: Set State -> Int -> Int
    go states 0 = maximum . map ((!!3) . fst) $ Set.elems states
    go states t = go states' (t - 1)
      where
        states'   = prune t
                  . setsMerge
                  . map (Set.fromList . nextStates blueprint)
                  $ Set.elems states

main :: IO ()
main = do
    blueprints <- map parseLine . lines <$> readFile "input.txt"

    -- Part 1
    print . sum
          . zipWith (*) [1..]
          . map (nbGeodes 24)
          $ blueprints

    -- Part 2
    print . foldl1 (*)
          . map (nbGeodes 32)
          $ take 3 blueprints

    print $ "Done."