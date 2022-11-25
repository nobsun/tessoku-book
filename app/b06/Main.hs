module Main where

import Data.Array ( (!), listArray, Array )
import Data.List ( scanl' )

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[String]] -> String
encode = unlines . map unwords

solve :: [[Int]] -> [[String]]
solve dds = case dds of
    [n]:as:[q]:qss -> map (query arr) qss
        where
            arr = listArray (0,n) (scanl' (+) 0 as)

query :: Array Int Int -> [Int] -> [String]
query arr [l,r]
    = case compare (r - l') (2 * (arr ! r - arr ! l')) of
        LT -> ["win"]
        EQ -> ["draw"]
        GT -> ["lose"]
    where
        l' = l - 1