module Main where

import Utils (tok, tuplify2)
import Debug.Trace (trace)

data Move = R | P | S deriving (Show, Eq, Enum)

-- Score for a single match.
score :: (Move, Move) -> Int
score (opponent, me) = shapeScore + outcomeScore
  where
    shapeScore   = 1 + fromEnum me
    outcomeScore = case (fromEnum opponent - fromEnum me + 3) `rem` 3 of
        1 -> 0
        0 -> 3
        2 -> 6

-- Parse input for part 1.
parseInput :: String -> [(Move, Move)]
parseInput = map (\ (opp, me) -> (case opp of {"A" -> R; "B" -> P; "C" -> S},
                                  case me  of {"X" -> R; "Y" -> P; "Z" -> S}))
           . map (tuplify2 . tok " ")
           . lines

-- Parse input for part 2.
parseInput' :: String -> [(Move, Move)]
parseInput' = map (\ (opp, result) -> (opp, case result of 
                                            "X" -> case opp of {R -> S; P -> R; S -> P}
                                            "Y" -> opp
                                            "Z" -> case opp of {R -> P; P -> S; S -> R}
                                       ))
            . map (\ (opp, result) -> (case opp of {"A" -> R; "B" -> P; "C" -> S}, result))
            . map (tuplify2 . tok " ")
            . lines

main :: IO ()
main = do
    input <- readFile "input.txt"

    print $ sum $ map score $ parseInput input

    print $ sum $ map score $ parseInput' input

    print $ "Done."