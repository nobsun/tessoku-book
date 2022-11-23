module Main where

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[Int]] -> String
encode = unlines . map (unwords . map show)

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [n,k]:_ -> [[ count n k]]

count :: Int -> Int -> Int
count n k = length [ () | r <- ns, b <- ns
                        , let w = k - (r + b)
                        , 1 <= w, w <= n ]
    where
        ns = [1 .. n]
