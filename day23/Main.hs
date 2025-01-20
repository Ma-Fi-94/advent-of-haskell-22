module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Grid as Grid
import Utils (singletons)

data Direction = N | S | W | E deriving (Show, Eq)
type Row       = Int
type Column    = Int
type Elf       = (Row, Column)

-- Rotate head to the end of a list.
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]

-- Does exactly what it says on the tin.
parseInput :: String -> Set Elf
parseInput = Set.fromList
           . map fst
           . filter ((=='#') . snd)
           . Grid.enumerate
           . Grid.fromList
           . lines

-- In the smallest orthogonal rectangle that encloses all elves count
-- the number of free spaces.
countEmpty :: Set Elf -> Int
countEmpty set = (rmax - rmin + 1) * (cmax - cmin + 1) - Set.size set
  where
    (rs, cs)     = (Set.map fst set, Set.map snd set)
    (rmin, rmax) = (Set.findMin rs, Set.findMax rs)
    (cmin, cmax) = (Set.findMin cs, Set.findMax cs)

-- Propose a new position for one elf at the given coordinate position
-- and given priority list of directions
propose :: Set Elf -> [Direction] -> Elf -> Elf
propose elves directions elf@(r, c)
    | allFree moore                          = elf
    | null directions                        = elf
    | head directions == N && allFree norths = (r - 1, c)
    | head directions == S && allFree souths = (r + 1, c)
    | head directions == W && allFree wests  = (r, c - 1)
    | head directions == E && allFree easts  = (r, c + 1)
    | otherwise                              = propose elves (tail directions) elf
  where
    allFree = all (`Set.notMember` elves)
    moore   = [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1),
               (r,     c - 1),             (r,     c + 1),
               (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]
    norths  = take 3 moore
    souths  = drop 5 moore
    wests   = [moore !! 0, moore !! 3, moore !! 5]
    easts   = [moore !! 2, moore !! 4, moore !! 7]

-- One round of movement
step :: Set Elf -> [Direction] -> Set Elf
step elves directions = Set.fromList (go posOld posProp)
  where
    posOld       = Set.elems elves
    posProp      = map (propose elves directions) posOld
    posPropValid = Set.fromList . singletons $ posProp
    go []         []                 = []
    go (old:olds) (new:news)
        | new `Set.member` posPropValid = new : go olds news
        | otherwise                     = old : go olds news

-- N subsequent rounds of movements
stepN :: Int -> Set Elf -> [Direction] -> Set Elf
stepN 0 elves directions = elves
stepN n elves directions = stepN (n - 1) elves' directions'
  where
    elves'      = step elves directions
    directions' = rotate directions

------------
-- Part 2 --
------------

-- Find the number of steps until convergence.
stepsUntilConv :: Set Elf -> [Direction] -> Int
stepsUntilConv elves directions = go 0 elves directions
  where
    go i set dirs
        | set == set' = i + 1
        | otherwise   = go (i + 1) set' dirs'
      where
        set'  = step set dirs
        dirs' = rotate dirs

main :: IO ()
main = do
    elves         <- parseInput <$> readFile "input.txt"
    let directions = [N, S, W, E]

    -- Part 1
    print $ countEmpty . stepN 10 elves $ directions

    -- Part 2
    print $ stepsUntilConv elves directions

    print $ "Done."

