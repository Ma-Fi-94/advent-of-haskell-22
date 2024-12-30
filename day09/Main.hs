module Main where

import Utils (readInt, tok)
import Data.Set (Set)
import qualified Data.Set as Set

type Command = Char
type Coord   = (Int, Int)
type Head    = Coord
type Tail    = Coord
type State   = (Head, Tail)

parseInput :: String -> [Command]
parseInput = concatMap (\ (tok1:tok2:_) -> replicate (readInt tok2) (head tok1))
           . map (tok " ")
           . lines

step :: State -> Command -> State
step state command = reactTail . moveHead command $ state

moveHead :: Command -> State -> State
moveHead c ((x, y), (xx, yy)) = case c of
    'U' -> ((x, y + 1), (xx, yy))
    'D' -> ((x, y - 1), (xx, yy))
    'L' -> ((x - 1, y), (xx, yy))
    'R' -> ((x + 1, y), (xx, yy))

reactTail :: State -> State
reactTail ((x, y), (xx, yy))
    | dist == (0, 0)                   = ((x, y), (xx, yy))
    | abs xdist <= 1 && abs ydist <= 1 = ((x, y), (xx, yy))
    | dist == (2, 0)                   = ((x, y), (xx + 1, yy))
    | dist == (-2, 0)                  = ((x, y), (xx - 1, yy))
    | dist == (0, 2)                   = ((x, y), (xx, yy + 1))
    | dist == (0, -2)                  = ((x, y), (xx, yy - 1))
    | xdist > 0 && ydist > 0           = ((x, y), (xx + 1, yy + 1))
    | xdist > 0 && ydist < 0           = ((x, y), (xx + 1, yy - 1))
    | xdist < 0 && ydist > 0           = ((x, y), (xx - 1, yy + 1))
    | xdist < 0 && ydist < 0           = ((x, y), (xx - 1, yy - 1))
  where
    dist           = (xdist, ydist)
    (xdist, ydist) = (x - xx, y - yy)


-- Part 2 --

-- Like step, but instead the state changes based on the new
-- position of the head.
step' :: State -> Head -> State
step' ((_, _), (xx, yy)) (x, y) = reactTail ((x, y), (xx, yy))


main :: IO ()
main = do
    commands <- parseInput <$> readFile "input.txt"

    let tailHistory = map snd $ scanl step ((0, 0), (0, 0)) commands

    -- Part 1
    print $ Set.size
          . Set.fromList
          $ tailHistory

    -- Part 2: Just do Part 1 eight more times
    print $ Set.size
          . Set.fromList
          . (!!8)
          . iterate (map snd . scanl step' ((0, 0), (0, 0)))
          $ tailHistory

    print $ "Done."

