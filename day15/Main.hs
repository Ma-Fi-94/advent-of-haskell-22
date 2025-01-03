module Main where

import qualified Data.Set as Set
import Data.List ((\\))
import Utils (readInt, tok)

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

-- Given a line number and one reading, calculate which columns in that
-- row are free. Note that we need to exclude the point of the beacon
-- if the beacon lies on the line we examine.
findFreeCols                        :: Int -> Reading -> [Int]
findFreeCols y ((sx, sy), (bx, by)) = if   y == by
                                      then xs \\ [bx]
                                      else xs
  where
    r     = cab (sx, sy) (bx, by)
    dy    = abs (y - sy)
    dxMax = r - dy
    xs    = [(sx - dxMax) .. (sx + dxMax)]

main :: IO ()
main = do
    readings <- map parseLine . lines <$> readFile "input.txt"

    -- Part 1
    print $ Set.size . Set.unions . map Set.fromList 
          $ map (findFreeCols 2000000) readings


    print $ "Done."
