module Main where

import Data.Array ( accumArray, elems )

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[Int]] -> String
encode = unlines . map (unwords . map show)

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [d]:[n]:lrs -> [ [a] | a <- scanl1 (+) diffs ]
        where
            diffs = take d 
                  $ elems 
                  $ accumArray (+) 0 (1,d+1)
                  $ concatMap conv lrs
            conv [l,r] = [(l,1),(r+1,-1)]
