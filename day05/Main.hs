module Main where

import Data.List (transpose)
import Utils (readInt, tok, tuplify3)

type Stack = [Char]
type Move  = (Int, Int, Int)

parseInput :: String -> ([Stack], [Move])
parseInput input = (stacks, moves)
  where
    moves          = map parseMoveLine moveLines
    stacks         = map (dropWhile (==' '))
                   . transpose
                   $ map parseStackLine stackLines

    stackLines     = filter ('[' `elem`) . lines $ input
    moveLines      = filter ('m' `elem`) . lines $ input

parseMoveLine :: String -> Move
parseMoveLine = tuplify3
              . map readInt
              . (\ t -> [t !! 1, t !! 3, t !! 5])
              . tok " "

parseStackLine :: String -> String
parseStackLine [] = []
parseStackLine xs = x' : parseStackLine xs'
  where
    x'  = head . drop 1 . take 2 $ xs
    xs' = drop 4 xs


mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt i f xs = (take i xs) ++ [f (xs !! i)] ++ (drop (i + 1) xs)

doMove :: [Stack] -> Move -> [Stack]
doMove stacks (n, source, destination) = push . pop $ stacks
  where
    pop   = mapAt (source - 1)      (drop n)
    push  = mapAt (destination - 1) ((reverse block)++)
    block = take n (stacks !! (source - 1))

doMove' :: [Stack] -> Move -> [Stack]
doMove' stacks (n, source, destination) = push . pop $ stacks
  where
    pop   = mapAt (source - 1)      (drop n)
    push  = mapAt (destination - 1) (block++)
    block = take n (stacks !! (source - 1))

main :: IO ()
main = do
    (stacks, moves) <- parseInput <$> readFile "input.txt"

    print $ map head $ foldl doMove stacks moves

    print $ map head $ foldl doMove' stacks moves

    print $ "Done."