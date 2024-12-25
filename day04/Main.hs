module Main where

import Utils (readInt, tok)

type ClosedInterval = (Int, Int)

-- Parses an input line into a 2-tuple of closed intervals.
parseLine :: String -> (ClosedInterval, ClosedInterval)
parseLine line = ((numbers !! 0, numbers !! 1),
                  (numbers !! 2, numbers !! 3))
  where
    numbers = map readInt $ tok "-," line

-- Check whether any of the given closed intervals fully contains the other.
contains :: ((ClosedInterval, ClosedInterval)) -> Bool
contains ((a, b), (c, d))
    | c >= a && d <= b = True
    | a >= c && b <= d = True
    | otherwise        = False

-- Check whether the two given closed intervals overlap
overlap :: ((ClosedInterval, ClosedInterval)) -> Bool
overlap ((a, b), (c, d))
    | a <= c && b >= c = True
    | c <= a && d >= a = True
    | otherwise        = False

main :: IO ()
main = do
    intervalPairs <- map parseLine . lines <$> readFile "input.txt"

    print $ length . filter contains $ intervalPairs

    print $ length . filter overlap $ intervalPairs

    print $ "Done."