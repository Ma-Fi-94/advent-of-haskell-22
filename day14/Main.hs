module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils (map2, readInt, tok, tuplify2)

-- Some sugar
type Coord = (Int, Int)
type Rock  = Coord
type Sand  = Coord

-- Parse an input line into a list of coordinate-tuples,
-- which denote the corners of the series of rock lines.
parseLine :: String -> [Rock]
parseLine  = map (tuplify2 . (map readInt) . tok ",")
           . tok "-> "

-- Given the corners of a series of rock lines, find all its
-- coordinates. This will produce some doublings, but since
-- we will store all coordinates in a Set, this doesn't matter.
fillLine                       :: [Rock] -> [Rock]
fillLine (_:[])                 = []
fillLine ((x1,y1):(x2,y2):rest) = current ++ fillLine ((x2,y2):rest)
  where
    current
        | x1 == x2 = [(x1, y) | y <- [y1..y2] ++ [y2..y1]]
        | y1 == y2 = [(x, y1) | x <- [x1..x2] ++ [x2..x1]]

-- Given the maximum y-level and the set of rock positions
-- we drop a block of sand until it maybe comes to rest. If it
-- instead flows into the infinite void, we return Nothing.
-- We could theoretically get ymax from rocks, but querying this
-- over and over again would decrease speed, hence we indulge
-- ourself in spending an extra parameter.
drop1                                  :: Int -> Set Rock -> Sand -> Maybe Sand
drop1 ymax rocks (x, y)
    | y > ymax                             = Nothing
    | (x, y + 1)     `Set.notMember` rocks = drop1 ymax rocks (x,     y + 1)
    | (x - 1, y + 1) `Set.notMember` rocks = drop1 ymax rocks (x - 1, y + 1)
    | (x + 1, y + 1) `Set.notMember` rocks = drop1 ymax rocks (x + 1, y + 1)
    | otherwise                            = Just (x, y)


-- Now, we add blocks of sand until dropSand returns Nothing,
-- and return how many blocks we were able to place.
-- To this end, we also need to find ymax first.
part1 :: Set Rock -> Int
part1 rocks = go rocks
  where
    ymax         = maximum . map snd $ Set.elems rocks
    go occupieds = case (drop1 ymax occupieds (500, 0)) of
        Just (x, y) -> 1 + go (Set.insert (x, y) occupieds)
        Nothing     -> 0

------------
-- Part 2 --
------------

-- Like before, but now we have a floor-line which stretches
-- out infinitely. Thus, every block can come to rest on it.
-- If we're not on the floor line yet, we again try the coordinates
-- below, below-left and below-right. If none of these work,
-- we know we cannot move any further. It remain to check, whether
-- we are blocking the entry -- if so, we terminate with Nothing.
-- If not, the sand just comes to rest where it currently is.

drop2                                     :: Int -> Set Rock -> Sand -> Maybe Sand
drop2 floorLine rocks (x, y)
    | y == floorLine - 1                   = Just (x, y)
    | (x, y + 1)     `Set.notMember` rocks = drop2 floorLine rocks (x,     y + 1)
    | (x - 1, y + 1) `Set.notMember` rocks = drop2 floorLine rocks (x - 1, y + 1)
    | (x + 1, y + 1) `Set.notMember` rocks = drop2 floorLine rocks (x + 1, y + 1)
    | (x, y) == (500, 0)                   = Nothing
    | otherwise                            = Just (x, y)

-- Like in Part 1, but now we calculate the position of the floorline.
-- Also, we need to add one to the result now, because we can very
-- well place the last block of sand that clogs the entry hole.
part2 :: Set Rock -> Int
part2 rocks = go rocks
  where
    floorLine    = (+2) . maximum . map snd $ Set.elems rocks
    go occupieds = case (drop2 floorLine occupieds (500, 0)) of
        Just (x, y) -> 1 + go (Set.insert (x, y) occupieds)
        Nothing     -> 1

main :: IO ()
main = do

    rocks <- Set.fromList
           . concatMap (fillLine . parseLine)
           . lines
         <$> readFile "input.txt" 

    print $ part1 rocks

    print $ part2 rocks


    print $ "Done."

