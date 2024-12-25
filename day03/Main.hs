module Main where

import Data.Char (isLowerCase, isUpperCase, ord)
import Data.List (intersect)
import Utils (groupn)

-- Convert character to priority score.
prio :: Char -> Int
prio c
    | isLowerCase c = ord c - ord 'a' + 1
    | isUpperCase c = ord c - ord 'A' + 27

-- Find the character that occurs in both halves of a given string.
-- We assume it's unique, based on the task.
findCommon :: String -> Char
findCommon string = head . filter (`elem` half2) $ half1
  where
    halflength = (length string) `div` 2
    half1      = take halflength string
    half2      = drop halflength string

-- Find the character that occurs in a list of strings.
-- We assume it's unique, based on the task.
findCommon' :: [String] -> Char
findCommon' = head . foldl1 intersect



main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"

    print $ sum . map (prio . findCommon) $ inputLines

    print $ sum . map (prio . findCommon') . groupn 3 $ inputLines

    print $ "Done."