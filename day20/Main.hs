module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Utils (mapAt, pop, push, readInt, tok)


-- Some sugar for clarity
type Index = Int
type Delta = Int

-- Helper to get the positive modulus of a number,
-- which we need to calculate the new index of a
-- list element
(%%) :: Int -> Int -> Int
a %% b
    | a >= 0 = a `rem` b
    | a < 0  = (a `rem` b) + b

-- Shift element at given index by the given delta.
shift :: Index -> Delta -> [a] -> [a]
shift _   0     xs = xs
shift idx delta xs = push idx' (xs !! idx)
                   . pop idx
                   $ xs
  where
    n    = length xs
    idx'
        | (idx + delta) > 0 && (idx + delta) < n = idx + delta
        | delta > 0 && (idx + delta) >= n        = (idx + delta) %% (n - 1)
        | delta < 0 && (idx + delta) == 0        = n - 1
        | delta < 0 && (idx + delta) < 0         = (idx + delta) %% (n - 1)

-- n rounds of mixing
mixN :: Int -> [Int] -> [Int]
mixN n xs = map snd . (!!n) . iterate (go 0) $ (zip [0..] xs)
  where
    go ctr xs
        | ctr == length xs = xs
        | otherwise        = go (ctr + 1) xs'
          where
            curEntry = head . filter (\ t -> fst t == ctr) $ xs
            idx      = fromJust $ elemIndex curEntry xs
            delta    = snd curEntry 
            xs'      = shift idx delta xs



-- Find the coordinates after mixing
coords :: [Int] -> Int
coords xs = foldl1 (+) [xs !! idx1, xs !! idx2, xs !! idx3]
  where
    zeroIdx = fromJust $ elemIndex 0 xs
    idx1    = (zeroIdx + 1000) `mod` length xs
    idx2    = (zeroIdx + 2000) `mod` length xs
    idx3    = (zeroIdx + 3000) `mod` length xs



main :: IO ()
main = do
    input <- map readInt . lines <$> readFile "input.txt"

    print . coords
          . mixN 1
          $ input

    print . coords
          . mixN 10
          . map (*811589153)
          $ input

    print $ "Done."

