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
    [t]:[n]:lrs -> [ [m] | m <- scanl1 (+) diffs ]
        where
            diffs = take t
                  $ elems
                  $ accumArray (+) 0 (0,t)
                  $ concatMap conv lrs
            conv [l,r] = [(l, 1), (r, -1)]
