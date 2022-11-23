module Main where

import Data.Array ( listArray, (!) )
import Data.List ( scanl' )

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[Int]] -> String
encode = unlines . map (unwords . map show)

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [n,q]:as:qss -> map query qss
        where
            db = listArray (0, n) (scanl' (+) 0 as)
            query [l,r] = [ db ! r - db ! (l - 1) ]
