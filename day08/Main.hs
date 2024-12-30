module Main where

import Utils (map2, readInt, takeWhile1)
import Data.List (reverse, transpose)

-----------------
-- Sugar shock --
-----------------

type Height  = Int

type Visible = Bool
type Tree    = (Height, Visible)
type Row     = [Tree]
type Field   = [Row]

type Score   = Int
type Tree'   = (Height, Score)
type Row'    = [Tree']
type Field'  = [Row']


-- Helper to map a function across lines and columns,
-- forwards and backwards.
map4 :: ([a] -> [a]) -> [[a]] -> [[a]]
map4 f xs = transpose . map (reverse . f . reverse) . transpose
          . transpose . map f . transpose
          . map (reverse . f . reverse)
          . map f
          $ xs

----------------------------------
-- Parsing stuff for both parts --
----------------------------------

-- Parse tree heights to a 2d Grid of tree height and visibility.
-- Visibility is initialised with False. Once a tree is marked
-- as visible, we cannot unmark it anymore.
parseLine :: String -> Row
parseLine  = map (\ char -> (readInt [char], False))

-- Parse tree heights to a 2d Grid of tree height and scenic score.
parseLine' :: String -> Row'
parseLine'  = map (\ char -> (readInt [char], 1))


------------------------------------
-- Core functions for both parts. --
------------------------------------

-- Given a single list of trees, determine which are visible when
-- looking from the left end of the list.
rowVisibilities :: Row -> Row
rowVisibilities = go (-1)
  where
    go _          []                        = []
    go curMaximum ((height, visible):trees) = current' : go curMaximum' trees
      where
        current'    = (height, visible || height > curMaximum)
        curMaximum' = maximum [curMaximum, height]

-- Given a single list of trees, determine the scenic scores for each,
-- looking left to right
rowScores :: Row' -> Row'
rowScores [] = []
rowScores ((height, score):trees) = current' : rowScores trees
  where
    factor   = length . takeWhile1 ((<height) . fst) $ trees
    current' = (height, score * factor) 





main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"

    print $ length
          . filter snd
          . concat
          . map4 rowVisibilities
          . map parseLine
          $ inputLines

    print $ maximum
          . map snd
          . concat
          . map4 rowScores
          . map parseLine'
          $ inputLines


    print $ "Done."

