module Main where

import Control.Arrow ((***))
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Utils (readInt, tok, tuplify2)

--------------------------------------------------------
-- A custom Packet datatype which is instance of Ord. --
--------------------------------------------------------

data Packet = Imm Int | List [Packet] deriving (Eq, Show)

instance Ord Packet where
    -- Trivial case: compare two Int
    Imm a   `compare` Imm b   = a `compare` b 

    -- Two empty lists are equal, and any empty list is
    -- smaller than any full one.
    List [] `compare` List [] = EQ
    List [] `compare` List _  = LT
    List _  `compare` List [] = GT

    -- To compare an immediate value and a list,
    -- the former is put into a list.
    Imm l   `compare` List r  = List [Imm l] `compare` List r
    List l  `compare` Imm  r  = List l       `compare` List [Imm r]

    -- Two lists are compared element-wise left-to-right.
    -- We could write some recursive function call here, however,
    -- we make use of the fact that Ordering is a Monoid and its
    -- `mappend` (syn.: <>) does exactly that already.
    List (l:ls) `compare` List (r:rs) = (l `compare` r) <> ls `compare` rs 

-------------
-- Parsing --
-------------

-- Parsing a string to a single packet.
-- This is strongly based on the following github repo,
-- but a bit edited for better readability: 
-- https://github.com/clatisus/advent-of-code-y2022/blob/master/src/Day13.hs
parsePacket :: Parser Packet
parsePacket =
    -- One or more digits. This may or may not fail, thus we need to fmap our
    -- attempt at reading this to an integer and prefixing with "Imm" constructor
    fmap (Imm . readInt) (P.many1 P.digit)
      -- or
      P.<|>
    -- Between [], packets separated by a comma
    -- Again, this may or may not fail, thus we need to fmap our attempt
    -- at prefixing the result with the "List" constructor
    fmap (List) (P.between (P.char '[') (P.char ']') (parsePacket `P.sepBy` (P.char ',')))

-- Parsing the complete input
parseInput :: P.SourceName -> String -> Either P.ParseError [(Packet, Packet)]
parseInput = P.parse $
    -- Merge two blocks to a 2-tuple by fmap-ing the (,) operator.
    -- For both blocks we skip the newline at the end of the string,
    -- Between every pair of blocks, there is exactly one newline as separator.
    ((,) <$> (parsePacket <* P.newline) <*> (parsePacket <* P.newline)) `P.sepBy1` P.newline


main :: IO ()
main = do
    input       <- readFile "input.txt"

    let packages = parseInput "" input
    case packages of
        Left err          -> print err
        Right packetPairs -> do

            -- Part 1
            print . sum
                  . map fst 
                  . filter (\ (_, (p1, p2)) -> p1 `compare` p2 == LT)
                  $ zip [1..] packetPairs

            -- Part 2
            let dividers = List [List [Imm 2]] : List [List [Imm 6]] : []

            print . uncurry (*)
                  . (\ (i1, i2) -> (fromJust i1 + 1, fromJust i2 + 1))
                  . (\ list -> (elemIndex (dividers !! 0) list,
                                elemIndex (dividers !! 1) list))
                  . sort
                  $ map fst packetPairs ++ map snd packetPairs ++ dividers

    print $ "Done."

