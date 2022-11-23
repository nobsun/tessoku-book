module Main where

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[String]] -> String
encode = unlines . map unwords

solve :: [[Int]] -> [[String]]
solve dds = case dds of
    [n]:as:_ -> [[ judge as ]]

judge :: [Int] -> String
judge as = case [ () | cs <- comb as 3, sum cs == 1000 ] of
    [] -> "No"
    _  -> "Yes"

comb :: [a] -> Int -> [[a]]
comb xs     0 = [ [] ]
comb []     _ = []
comb (x:xs) k = [ x : ys | ys <- comb xs (k - 1) ]
             ++ comb xs k

