module Main where

import Data.List (nub)

findWindow :: Int -> String -> Int
findWindow n string = go n (take n string) (drop n string)
  where
    go i lastn rest
        | lastn == nub lastn = i
        | otherwise          = go (i + 1) lastn' rest'
      where
        lastn' = (tail lastn) ++ [head rest]
        rest'  = tail rest

main :: IO ()
main = do
    fileContents <- readFile "input.txt"

    print $ findWindow 4 fileContents

    print $ findWindow 14 fileContents

    print $ "Done."

