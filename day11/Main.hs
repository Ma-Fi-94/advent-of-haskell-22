module Main where

import Data.Char (isDigit)
import Data.List (isSuffixOf, sort)
import Utils (dropNondigits, dropWhile1, fou4, mapAt, readInt, tok)

-------------------------------------------
-- A Monkey type and some helpers for it --
-------------------------------------------

type Monkey     = ([WorryLevel], NewWorry, NewID, Count)
type MonkeyID   = Int
type WorryLevel = Int
type NewWorry   = (WorryLevel -> WorryLevel)
type NewID      = (WorryLevel -> MonkeyID)
type Count      = Int

-- Increase the count of inspected items
increaseCountBy :: Int -> Monkey -> Monkey
increaseCountBy i (ws, f1, f2, c) = (ws, f1, f2, c + i)


-- Give one items (specified by worry-level) to the given monkey
give :: WorryLevel -> Monkey -> Monkey
give w (ws, f1, f2, c) = (ws ++ [w], f1, f2, c) 

-- Remove the first item of the given monkey
removeFirst :: Monkey -> Monkey
removeFirst ((w:ws), f1, f2, c) = (ws, f1, f2, c)

-- Execute a throw, given the idea of the throwing monkey,
-- the current list of monkeys and the specified throw.
throwFrom :: MonkeyID -> [Monkey] -> (MonkeyID, WorryLevel) -> [Monkey]
throwFrom from monkeys (to, worryLevel) = mapAt to (give worryLevel)
                                        . mapAt from removeFirst 
                                        $ monkeys

-------------
-- Parsing --
-------------

-- Part 1
parseMonkey :: [String] -> Monkey
parseMonkey entryLines = (items, newWorry, newID, 0)
  where
    items    = map readInt . tok ", " . dropWhile1 (/=':') $ entryLines !! 1
    newWorry = if   "old" `isSuffixOf` (entryLines !! 2)
               then (\ old -> (old `operator` old) `div` 3)
               else (\ old -> (old `operator` operand) `div` 3)
      where
        operator | '+' `elem` (entryLines !! 2) = (+)
                 | '*' `elem` (entryLines !! 2) = (*)
        operand  = readInt . dropNondigits $ entryLines !! 2
    newID i   = if   (newWorry i) `rem` divisor == 0
                then idTrue
                else idFalse
      where
        divisor = readInt . dropNondigits $ entryLines !! 3
        idTrue  = readInt . dropNondigits $ entryLines !! 4
        idFalse = readInt . dropNondigits $ entryLines !! 5


-- Part 2.
-- The key insight here is that we if we were to just use the
-- previous approach, numbers would grow huge, so we'd require
-- Integers, as opposed to Ints, which are slow.
-- But because we don't integer-divide by 3 anymore, and we only
-- care about divisibilities where all divisors are prime numbers,
-- we can perform the update operation of the worry level modulo
-- all possible primes.
-- This is because if p divides some n, then p will also divide
-- n mod p.
parseMonkey' :: [String] -> Monkey
parseMonkey' entryLines = (items, newWorry, newID, 0)
  where
    items      = map readInt . tok ", " . dropWhile1 (/=':') $ entryLines !! 1
    newWorry   = if   "old" `isSuffixOf` (entryLines !! 2)
                 then (\ old -> (old `operator` old) `mod` somePrimes)
                 else (\ old -> (old `operator` operand) `mod` somePrimes)
      where
        operator   | '+' `elem` (entryLines !! 2) = (+)
                   | '*' `elem` (entryLines !! 2) = (*)
        operand    = readInt . dropNondigits $ entryLines !! 2
        somePrimes = 2*3*5*7*11*13*17*19
    newID i   = if   (newWorry i) `rem` divisor == 0
                then idTrue
                else idFalse
      where
        divisor = readInt . dropNondigits $ entryLines !! 3
        idTrue  = readInt . dropNondigits $ entryLines !! 4
        idFalse = readInt . dropNondigits $ entryLines !! 5


--------------------------------
-- The actual programme logic --
--------------------------------

-- Evaluate a given monkey, return a list of tuples, which
-- denote which monkey will obtains which items
getThrows :: Monkey -> [(MonkeyID, WorryLevel)]
getThrows ([], _, _, _)                     = []
getThrows (worries, newWorry, newID, count) = map (\ worry -> (newID worry, newWorry worry)) worries


-- Calculate one entire round
oneRound :: [Monkey] -> [Monkey]
oneRound = go 0
  where
    go index monkeys
        | index == length monkeys = monkeys
        | otherwise               = go (index + 1) monkeys'
      where
        currentMonkey        = monkeys !! index
        currentThrows        = getThrows currentMonkey
        monkeys'             = mapAt index (increaseCountBy (length currentThrows))
                             $ foldl (throwFrom index) monkeys currentThrows



main :: IO ()
main = do
    blocks <- tok [""] . lines <$> readFile "input.txt"

    print . product
          . take 2
          . reverse
          . sort
          . map fou4
          . (!!20)
          . iterate oneRound
          . map parseMonkey
          $ blocks

    print . product
          . take 2
          . reverse
          . sort
          . map fou4
          . (!!10000)
          . iterate oneRound
          . map parseMonkey'
          $ blocks

    print $ "Done."

