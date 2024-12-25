module Main where

import Data.List (sort)
import Utils (map2, readInt, tok)

main :: IO ()
main = do
    elves <- reverse . sort . map sum         -- sorted [Int]
           . map2 readInt . tok [""] . lines  -- parse to [[Int]]
         <$> readFile "input.txt"             -- read input

    print $ head elves

    print $ sum . take 3 $ elves

    print $ "Done."