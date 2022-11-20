module Main where

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[String]] -> String
encode = unlines . map unwords

solve :: [[Int]] -> [[String]]
solve dds = case dds of
    [a,b]:_ -> [[iter a]]
        where
            iter n
                | n > b            = "No"
                | 100 `mod` n == 0 = "Yes"
                | otherwise        = iter (n + 1)
