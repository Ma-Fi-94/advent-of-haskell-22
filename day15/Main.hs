module Main where

import Control.Arrow (second)
import Data.List (sort)
import Range (Range(..))
import qualified Range as R
import Utils (readInt, tok, map2)

type Coord   = (Int, Int)
type Sensor  = Coord
type Beacon  = Coord
type Reading = (Sensor, Beacon)

-- Parse an input line into a Reading object.
parseLine     :: String -> Reading
parseLine line = ((nbs !! 0, nbs !! 1), (nbs !! 2, nbs !! 3))
  where
    nbs = map readInt . tok "=" . filter (`elem` "0123456789=-") $ line

-- Taxicab distance between two points.
cab                  :: Coord -> Coord -> Int
cab (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- The number of elements in an Int Range
nbElems :: Range Int -> Int
nbElems (R a b)
    | a <= b    = b - a + 1
    | otherwise = a - b + 1

-- Given a line number and ONE reading, calculate which columns in that
-- row are free. Note that we need to exclude the point of the beacon
-- if the beacon lies on the line we examine.
findFreeCols :: Int -> Reading -> [Range Int]
findFreeCols y ((sx, sy), (bx, by))
    | dxMax <= 0 = []
    | y == by    = bx `R.remove` R (sx - dxMax) (sx + dxMax)
    | otherwise  = [R.range (sx - dxMax) (sx + dxMax)]
  where
    r     = cab (sx, sy) (bx, by)
    dy    = abs (y - sy)
    dxMax = r - dy

------------
-- Part 2 --
------------

-- We now need a slightly different version of findFreeCols, where
-- points with beacons are not excluded, since the distressed beacon
-- can not stand on these.
findFreeCols' :: Int -> Reading -> [Range Int]
findFreeCols' y ((sx, sy), (bx, by))
    | dxMax <= 0 = []
    | otherwise  = [R.range (sx - dxMax) (sx + dxMax)]
  where
    r     = cab (sx, sy) (bx, by)
    dy    = abs (y - sy)
    dxMax = r - dy

-- Given a List of DISJUNCT Int Ranges, find all Gaps between them.
findGaps :: [Range Int] -> [Int]
findGaps ranges = go (sort ranges)
  where
    go ((R a b) : [])           = []
    go ((R a b) : (R c d) : rs) = [(b + 1) .. (c - 1)] ++ go ((R c d) : rs)

main :: IO ()
main = do
    readings <- map parseLine . lines <$> readFile "input.txt"


    -- Part 1: Only examine row 10 in particular.
    print . sum
          . map nbElems
          . R.disjunctify
          . concatMap (findFreeCols 10)
          $ readings

    -- Part 2: Like Part 1, but we examine all rows from 0 to 4000000.
    print . id -- (\ (y, x) -> 4000000 * x + y)
          . head
          . map (second head)
          . filter (\ (y, xs) -> xs /= [])
          . map (second findGaps)
          . map (second R.disjunctify)
          . map (\ y -> (y, concatMap (findFreeCols' y) readings))
          $ [0..4000000]


    print $ "Done."

