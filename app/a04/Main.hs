module Main where

import Data.Char ( intToDigit )

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[String]] -> String
encode = unlines . map unwords

solve :: [[Int]] -> [[String]]
solve dds = case dds of
    [n]:_ -> [[ int2binrep n ]]

int2binrep :: Int -> String
int2binrep = map intToDigit 
           . reverse 
           . take 10 
           . (++ repeat 0) 
           . int2bin

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)
