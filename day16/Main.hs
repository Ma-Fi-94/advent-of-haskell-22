module Main where

import Data.Char (isDigit, isUpper)
import Data.List (tails)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (dropNondigits, takeDigits, dropWhile1, readInt, tok)

----------------
-- Some sugar --
----------------

-- Basic types: Valve name, flow rate, amount of pressure released.
type ValveName  = String
type Rate       = Int
type Pressure   = Int

-- One valve entry: The name, its flowrate, and the tunnels to neighbours.
-- We use a nested tuple for easy conversion to a Map.
type Entry    = (ValveName, (Rate, [ValveName]))
type ValveMap = Map ValveName (Rate, [ValveName])

-- A game state: Current position, the set of opened valves and
-- the sum of pressure released so far. We use a nested tuple for
-- easy conversion to a Map.
type State    = ((ValveName, Set ValveName), Pressure)
type StateMap = Map (ValveName, Set ValveName) Pressure

-------------
-- Parsing --
-------------

parseLine :: String -> Entry
parseLine line = (valve, (rate, tunnels))
  where
    valve   = takeWhile (/=' ') . dropWhile1 (/=' ') $ line
    rate    = readInt . takeDigits . dropNondigits $ line
    tunnels = tok ", " . dropWhile (not . isUpper) . dropWhile1 (/=';') $ line

--------------------------------
-- The actual programme logic --
--------------------------------

-- Given the valveMap, the remaining amount of time, and a state, find all
-- possible next states. We can either open the current valve (which we
-- only do if its flow rate is larger zero and it's still closed),
-- or go through any of the tunnels.
steps :: ValveMap -> Int -> State -> [State]
steps valveMap t ((current, opens), pressure) = openCurrent ++ goAways
  where

    openCurrent = if   (current `Set.notMember` opens) && flowrate > 0
                  then [((current, opens'), pressure')]
                  else []
    flowrate    = fst $ valveMap Map.! current
    opens'      = current `Set.insert` opens
    pressure'   = pressure + (t - 1) * flowrate

    goAways     = [((current', opens), pressure) | current' <- tunnels]
    tunnels     = snd $ valveMap Map.! current

-- Given the valveMap and the allowed amount of time, return all possibilities
-- of valves we can open and how much pressure we release respectively.
run :: ValveMap -> Int ->  [((Set String), Int)]
-- We start at "AA", no valves opened yet (empty set), no pressure released so far
run valveMap tmax = go (Map.singleton ("AA", Set.empty) 0) tmax
  where
    -- No time left. Return the maximum amount of pressure released
    -- over all states.
    go stateMap 0 = map (\ ((_, opens), pressure) -> (opens, pressure))
                  . Map.assocs
                  $ stateMap

    -- If there's still time left, we consider all possible steps,
    -- and prune the state space at every step.
    go stateMap t = go stateMap' (t - 1)
      where
        stateMap' = Map.fromListWith max
                  . concatMap (steps valveMap t)
                  . Map.assocs
                  $ stateMap



main :: IO ()
main = do
    valveMap <- Map.fromList . map parseLine . lines <$> readFile "input.txt"

    -- Part 1: Calculate the sum of released pressure over all possible routes.
    -- We print the maximum, disregarding the Map keys.
    print . maximum
          . map snd
          $ run valveMap 30

    -- Part 2: Key idea here is to solve over 26 time steps and look at all pairs
    -- of disjoint sets of nodes, maximising the sum of released pressure
    let routeValues = run valveMap 26
    print (maximum [v1+v2 | (open1, v1) : elephants <- tails routeValues,
                            (open2, v2)             <- elephants,
                            Set.null (Set.intersection open1 open2)])


    print $ "Done."

