module Main where

import Control.Arrow ((&&&))
import Data.Either (lefts, rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (fst3, snd3, readInt, tok)

----------------
-- Some sugar --
----------------

type Name     = String
type Known    = (Name, Int)
type Unknown  = (Name, (Name, Name, (Int -> Int -> Int)))
type Knowns   = Map Name Int
type Unknowns = Map Name (Name, Name, (Int -> Int -> Int))

-------------
-- Parsing --
-------------

parseLine :: String -> Either Known Unknown
parseLine line = if   any (`elem` line) "+-*/"
                 then Right (parseUnknown line)
                 else Left  (parseKnown   line)
  where
    parseKnown l   = (name, value)
      where
        name  = toks !! 0
        value = readInt (toks !! 1)
        toks  = tok ": " l
    parseUnknown l = (name, (operand1, operand2, operator))
      where
        name     = toks !! 0
        operand1 = toks !! 1
        operand2 = toks !! 2
        operator
            | '+' `elem` l = (+)
            | '-' `elem` l = (-)
            | '*' `elem` l = (*)
            | '/' `elem` l = div
        toks     = tok ": +-*/" l

parseLines :: [String] -> (Knowns, Unknowns)
parseLines ls = (Map.fromList . lefts) &&& (Map.fromList . rights)
              $ map parseLine ls

----------------------
-- The actual logic --
----------------------

-- Solve all the unknowns.
solve :: (Knowns, Unknowns) -> Knowns
solve (knowns, unknowns)
    | Map.null unknowns = knowns
    | otherwise         = solve (knowns', unknowns')
  where
    unknowns' = Map.delete (fst current) unknowns
    knowns'   = Map.insert (fst current) (snd current) knowns
    current   = (\ (name, (opd1, opd2, op))
                     -> (name, (knowns Map.! opd1) `op` (knowns Map.! opd2)))
              . head
              . filter (\ (name, (opd1, opd2, op))
                            -> all (`Map.member` knowns) [opd1, opd2])
              $ Map.assocs unknowns

-- Part 2 --

-- Evaluate the two terms of "root", given an input for "humn"
evaluateRoot :: Int -> (Knowns, Unknowns) -> (Int, Int)
evaluateRoot humn (knowns, unknowns) = (Map.! name1) &&& (Map.! name2)
                                     $ solve (knowns', unknowns)
  where
    rootEntry = (Map.! "root") $ unknowns
    name1     = fst3 rootEntry
    name2     = snd3 rootEntry
    knowns'   = Map.insert "humn" humn $ knowns

-- Bisection algo
bisect :: (Int, Int) -> (Knowns, Unknowns) -> [Int]
bisect (a, b) (knowns, unknowns)
    | b - a <= 1 = [a .. b]
    | otherwise   = if   diffAtMid > 0
                    then bisect (mid, b) (knowns, unknowns)
                    else bisect (a, mid) (knowns, unknowns)
      where
        mid       = (a + b) `div` 2
        diffAtMid = uncurry (-) $ evaluateRoot mid (knowns, unknowns)

main :: IO ()
main = do
    (knowns, unknowns) <- parseLines . lines <$> readFile "input.txt"

    -- Part 1
    print . (Map.! "root")
          . solve
          $ (knowns, unknowns)

    -- Part 2
    -- Playing around with some values suggests that the second operand
    -- is constant, whereas the first one is monotonically decreasing in humn,
    -- however not strictly so. Hence, the difference decreases in humn.
    -- Further playing around yields very loose bound of 0, 10000000000000.
    -- The idea now is to bisect this interval successively, until we have
    -- less than 3 elements. Because we use integer division, only the
    -- smallest of the solutions will be the correct one.
    print . fst
          . head
          . filter ((==0) . snd)
          . map (\ h -> (h, uncurry (-) $ evaluateRoot h (knowns, unknowns))) 
          $ bisect (0, 10000000000000) (knowns, unknowns)

    print $ "Done."

