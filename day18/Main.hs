module Main where

import Utils (readInt, ordSingletons, tok, tuplify3, fst3, snd3, thi3)

import qualified Data.Set as Set
import Data.Set (Set)

-- A cube is identified by a tuple of its 3d coordinates.
-- A surface is identified by the two cubes that share it,
-- thus we model surfaces as tuple of two cubes, where
-- the first cube is <= the second following the "natural"
-- component-wise left-to-right order for n-tuples.
-- Having Ord, we can use Data.Map for fast counting
type X       = Int
type Y       = Int
type Z       = Int
type Cube    = (X, Y, Z)
type Surface = (Cube, Cube)

-- Get a cube's six surfaces.
surfaces :: Cube -> [Surface]
surfaces c@(x, y, z) = [left, right, back, front, up, down]
  where
    (left, right)    = (((x - 1, y, z), c), (c, (x + 1, y, z)))
    (back, front)    = (((x, y - 1, z), c), (c, (x, y + 1, z)))   
    (down, up)       = (((x, y, z - 1), c), (c, (x, y, z + 1)))     

-- Parse a line of input which contains a cube position
parseCube :: String -> Cube
parseCube = tuplify3 . map readInt . tok [',']

------------
-- Part 2 --
------------

type Boundingbox = ((X, X), (Y, Y), (Z, Z))

-- Get all of a cube's von Neumann neighbourhood cubes.
vonNeu :: Cube -> [Cube]
vonNeu (x, y, z) = [(x - 1, y, z), (x + 1, y, z),
                    (x, y - 1, z), (x, y + 1, z),
                    (x, y, z - 1), (x, y, z + 1)]

-- Construct a bounding box around the list of all cubes in the lava drop
boundingBox :: [Cube] -> Boundingbox
boundingBox cs = ((xmin, xmax), (ymin, ymax), (zmin, zmax))
  where
    (xs, ys, zs) = (map fst3 cs, map snd3 cs, map thi3 cs)
    (xmin, xmax) = (minimum xs - 1, maximum xs + 1)
    (ymin, ymax) = (minimum ys - 1, maximum ys + 1)
    (zmin, zmax) = (minimum zs - 1, maximum zs + 1)

-- Check whether a cube falls into a bounding box, including the box borders
inBound :: Boundingbox -> Cube -> Bool
inBound ((xmin, xmax), (ymin, ymax), (zmin, zmax)) (x,y,z) =
    (x >= xmin) && (x <= xmax) && 
    (y >= ymin) && (y <= ymax) && 
    (z >= zmin) && (z <= zmax)

-- Find the list of cubes of air around the lava drop using floodfilling.
-- Since we accumulate in a Set, we return just the Set.
outside :: [Cube] -> Set Cube
outside lavaList = go Set.empty [start]
  where
    box                   = boundingBox lavaList
    start                 = (fst (fst3 box), fst (snd3 box), fst (thi3 box))
    
    go accum []           = accum
    go accum queue@(q:qs) = go (q `Set.insert` accum) queue'
      where
        queue' = (++qs)
               . filter (inBound box)
               . filter (`Set.notMember` lava)
               . filter (`Set.notMember` accum)
               $ vonNeu q
        lava   = Set.fromList lavaList

main :: IO ()
main = do
    cubes <- map parseCube . lines <$> readFile "input.txt"

    -- Part 1: Count all surfaces of the lava blob,
    -- both outer and inner. Make it a Set for later.
    let lavaSurfaces = Set.fromList
                     . ordSingletons
                     . concatMap surfaces
                     $ cubes
    print $ Set.size lavaSurfaces
    
    -- Part 2: Use floodfilling to find all cubes in
    -- a bounding box around the blob. Find their surfaces
    -- and count how many of them are also part of lavaSurfaces,
    -- since these are necessarily the outer (but not inner)
    -- surfaces of the lava blob.
    let aroundSurfaces = Set.fromList
                       . ordSingletons
                       . concatMap surfaces
                       $ outside cubes 
    print $ length $ lavaSurfaces `Set.intersection` aroundSurfaces

    print $ "Done."

