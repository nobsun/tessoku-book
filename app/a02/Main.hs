module Main where

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[String]] -> String
encode = unlines . map unwords

solve :: [[Int]] -> [[String]]
solve dds = case dds of
    [n,x]:as:_
        | x `elem` as -> [["Yes"]]
        | otherwise   -> [["No"]]
