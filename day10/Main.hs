module Main where

import Utils (readInt, groupn)

type Register    = Int
data Instruction = NOP | ADDX Int deriving Show

parseLine :: String -> Instruction
parseLine s
    | s == "noop" = NOP
    | otherwise   = ADDX . readInt . drop 5 $ s

-- Run the provided instructions and return the values of the register
-- *during* (not after) every cycle
run :: [Instruction] -> [Register]
run = go 1
  where
    go reg []     = []
    go reg (i:is) = case i of
      NOP    -> reg : go reg is
      ADDX k -> reg : reg : go (reg + k) is

main :: IO ()
main = do
    registers <- run . map parseLine . lines <$> readFile "input.txt"

    -- Part 1
    print . sum
          . (\ list -> [list !! 19, list !! 59, list !! 99,
                        list !! 139, list !! 179, list !! 219])
          . zipWith (*) [1..]
          $ registers

    -- Part 2
    mapM_ print . groupn 40
                . map (\ d -> if d < 2 then '#' else '.') 
                . map (abs . uncurry (-))
                . zip (cycle [0..39])
                $ registers


    print $ "Done."

