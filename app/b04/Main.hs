module Main where

import Data.Char ( digitToInt )

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[String]]
decode =  map words . lines

encode :: [[Int]] -> String
encode = unlines . map (unwords . map show)

solve :: [[String]] -> [[Int]]
solve dds = case dds of
    [n]:_ -> [[ bin2int n ]]

bin2int :: String -> Int
bin2int = foldl phi 0
    where
        phi a b = 2 * a + digitToInt b
