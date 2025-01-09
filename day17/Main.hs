module Main where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tuple (swap)
import Utils (firstRecur)

-- Importantly, we define a coordinate as (y,x),
-- because Data.Set stores values using the "natural"
-- Ord defined on tuples, i.e. sorted by the first,
-- then the second element, and we'll have to do lots
-- of accesses based on y.
type Y       = Int
type X       = Int
type Coord   = (Y, X)
type Command = Char
type Shape   = Set Coord
type Board   = Set Coord

-- Find the board's highest occupied line.
ymax :: Board -> Y
ymax = maximum . map fst . Set.elems

-- Let a shape fall down one step.
fall :: Shape -> Shape
fall = Set.map (\(y, x) -> (y - 1, x))

-- The five possible shapes. Coordinates as a function of the board's
-- highest occupied row. Every shape appears with a distance of three
-- units above this row, and a distance of two units between its
-- left border and the left wall.
shapes :: Int -> [Shape]
shapes ymax = [minus, plus, ell, vertical, square]
  where
    ylo      = ymax + 4
    minus    = Set.fromList [(ylo, 2), (ylo, 3),   (ylo, 4),   (ylo, 5)]
    plus     = Set.fromList [(ylo, 3), (ylo+1, 2), (ylo+1, 3), (ylo+1, 4), (ylo+2, 3)]
    ell      = Set.fromList [(ylo, 2), (ylo, 3),   (ylo, 4),   (ylo+1, 4), (ylo+2, 4)]
    vertical = Set.fromList [(ylo, 2), (ylo+1, 2), (ylo+2, 2), (ylo+3, 2)]
    square   = Set.fromList [(ylo, 2), (ylo, 3),   (ylo+1, 2), (ylo+1, 3)]

-- Shift a shape according to a command character.
-- If the shape would bump into the side border or another
-- rock, we don't shift it.
tryShift :: Command -> Board -> Shape -> Shape
tryShift '<' b s
    | borderCollides || rockCollides = s
    | otherwise                      = s'
      where
        s'             = Set.map (\(y, x) -> (y, x - 1)) s
        borderCollides = any (\t -> snd t < 0) (Set.elems s')
        rockCollides   = not $ Set.disjoint s' b
        
tryShift '>' b s
    | borderCollides || rockCollides = s
    | otherwise                      = s'
      where
        s'             = Set.map (\(y, x) -> (y, x + 1)) s
        borderCollides = any (\t -> snd t > 6) (Set.elems s')
        rockCollides   = not $ Set.disjoint s' b

-- Add shape number i to a board, following provided shift commands.
-- Returns the remaining list of commands, and the updated board.
-- We also assume we can discard everything that's 50 lines below ymax,
-- to keep the Set small, and thus runtime short.
addShape :: ([Command], Board) -> Int -> ([Command], Board)
addShape (commands, board) shapeNb = go newShape commands board
  where
    newShape = shapes (ymax board) !! shapeNb
    go s (c:cs) b
        | cannotFall = (cs, finalBoard)
        | otherwise  = go s'' cs b
          where
            finalBoard = Set.filter (\(y, x) -> y > (ymax b) - 100)
                       $ Set.union s' b
            cannotFall = not . Set.null $ Set.intersection b s''
            s'         = tryShift c b s
            s''        = fall s'

-- Run on an initial board b exactly i steps using command string cs,
-- return the remaining commands and update board.
simulate :: Board -> [Command] -> Int -> ([Command], Board)
simulate board commands i = foldl addShape (commands, board)
                          $ take i (cycle [0..4])
------------
-- Part 2 --
------------

-- Like simulate, but with scanl and running infinitely.
simulateScan :: Board -> [Command] -> [([Command], Board)]
simulateScan board commands = scanl addShape (commands, board)
                            $ cycle [0..4]

-- Rolling differences
diffs xs = zipWith (-) (tail xs) xs

-- Shift up all rocks to a desired ymax
shiftTo :: Int -> Board -> Board
shiftTo newYmax board = Set.map (\ (y, x) -> (y + dy, x)) board
  where dy = newYmax - ymax board


main :: IO ()
main = do
    fileContent  <- readFile "input.txt"  
    let commands  = cycle . head . lines $ fileContent   
    let initBoard = Set.fromList [(-1, x) | x <- [0..6]]
    
    -- Part 1
    print . (+1) . ymax . snd $ simulate initBoard commands 2022

    -- Part 2
    
    -- Simulate infinitely many steps, drop a burn-in period of 1000
    -- elements, corresponding to calculating 999 falling rocks.
    let states = drop 1000
               $ simulateScan initBoard commands

    -- Calculate step-wise changes in height
    let deltas = diffs
               . map (ymax . snd)
               $ states

    -- Find cycle in deltas by searching for the first recurrence
    -- of a block of 100 subsequent height changes.
    -- Also find the overall change in height after one such cycle.
    -- Next, find the number n of complete cycles we can skip over
    -- Finally, calculate the number r of remaining rocks to simulate
    -- after we have skipped over all complete cycles.
    let (i, j) = fromJust
               . firstRecur
               $ map (\ i -> take 100 . drop i $ deltas) [0..]    
    let dh     = (ymax . snd) (states !! j) - (ymax . snd) (states !! i)
    let n      = (1000000000000 - 999) `div` (j - i)
    let r      = (1000000000000 - 999) `rem` (j - i)

    -- Hence, we take the i-th state after burn-in, find its ymax
    -- and add n * h units to this number.
    let stateBeforeCycles = states !! i 
    let ymaxAfterCycles   = (+(n * dh)) . ymax . snd $ stateBeforeCycles

    -- Now we only need to take the startState, shift all heights
    -- s.t. the maximum height corresponds to heightAfterCycles,
    -- and simulate r more rocks...
    let stateAfterCycles = (\ (commands, board) -> (commands, shiftTo ymaxAfterCycles board))
                         $ stateBeforeCycles

    -- ... and simulate r more rocks.
    print . ymax
          . snd
          . simulate (snd stateAfterCycles) (fst stateAfterCycles)
          $ r

    print $ "Done."

