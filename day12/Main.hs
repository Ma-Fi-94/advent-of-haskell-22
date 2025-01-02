module Main where

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map

import Grid (Grid, Coord, Grid(..))
import qualified Grid as G
import qualified WDGraph as WDG


-- Parse the input to a Grid of Int.
parseGrid :: String -> (Coord, Coord, Grid Int)
parseGrid input = (coordStart, coordEnd, intGrid)
  where
    charGrid   = G.fromList . lines $ input
    coordStart = fst . head . filter ((=='S') . snd) . G.enumerate $ charGrid
    coordEnd   = fst . head . filter ((=='E') . snd) . G.enumerate $ charGrid
    intGrid    = G.gridMap (\ c -> case c of
                                'S' -> 0
                                'E' -> 25
                                c   -> ord c - ord 'a')
               $ charGrid

-- From the parsed Grid, extract all edges.
extractEdges :: Grid Int -> [(Coord, [Coord])]
extractEdges grid = map (\ coord -> (coord, neighbours grid coord))
           . map fst
           $ G.enumerate grid


-- Find all suitable neighbours of a cell.
-- We only select directly adjacent cells,
-- with a height difference of 0 or +1.
neighbours :: Grid Int -> Coord -> [Coord]
neighbours grid coord = map fst
                      . filter (\ (_, height) -> (height - curHeight) <= 1)
                      $ G.vonNeum grid coord
  where
    curHeight = G.cell grid coord

-- We need this because WDGraph requires Int node-IDs
coordToNode :: Grid a -> Coord -> Int
coordToNode (Grid _ width _) (row, col) = row * width + col

------------
-- Part 2 --
------------

traceBackTo :: [Int] -> Map Int Int -> Int -> Int
traceBackTo terminals predecessors current
    | current `elem` terminals = 0
    | otherwise                = 1 + traceBackTo terminals predecessors current'
  where
    current' = predecessors Map.! current



main :: IO ()
main = do
    -- Read and parse input
    (coordStart, coordEnd, grid) <- parseGrid <$> readFile "input.txt"
    let edges                     = extractEdges grid

    -- Convert coordinates to node indices of type Int,
    -- since that's what my graph lib requires.
    let nodeStart = coordToNode grid coordStart
    let nodeEnd   = coordToNode grid coordEnd
    let nodeEdges = map (\ (coord, coords) -> (coordToNode grid coord,
                                               map (coordToNode grid) coords))
                  $ edges

    -- Construct the graph. All weights are set to one, since we only
    -- care about step counts.
    let graph = WDG.fromList
              . map (\ (node, nodes) -> (node, map (\ n -> (n, 1)) nodes))
              $ nodeEdges
    
    -- Part 1 --

    -- Run Dijkstra on the graph, and get the distance of the terminal node.
    let (distances, predecessors) = WDG.dijkstra graph nodeStart
    print $ distances Map.! nodeEnd

    -- Part 2 --
    -- The idea is to reuse the result from earlier, and trace back
    -- the path from the terminal node, until we reach the first valid
    -- starting node (i.e. a cell of height zero).
    let validStartNodes = map (coordToNode grid . fst)
                        . filter (\ (coord, height) -> height == 0)
                        $ G.enumerate grid
    print $ traceBackTo validStartNodes predecessors nodeEnd






    

    print $ "Done."

