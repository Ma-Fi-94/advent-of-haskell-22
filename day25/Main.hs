{-# LANGUAGE LambdaCase #-}

module Main where

snafu2dec :: String -> Int
snafu2dec = sum
          . zipWith (*) [5 ^ i | i <- [0..]]
          . map (\case '=' -> (-2)
                       '-' -> (-1)
                       '0' -> 0
                       '1' -> 1
                       '2' -> 2)
          . reverse

dec2snafu :: Int -> String
dec2snafu 0 = ""
dec2snafu n = dec2snafu n' ++ digits !! remainder
  where
    digits    = ["0", "1", "2", "=", "-"]
    remainder = n `mod` 5
    n'        = n `div` 5 + (if remainder > 2 then 1 else 0)

main :: IO ()
main = do

    fileContents <- readFile "input.txt"

    print . dec2snafu
          . sum
          . map snafu2dec
          . lines
          $ fileContents

    print $ "Done."

