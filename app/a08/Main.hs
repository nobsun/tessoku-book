module Main where

import Data.Array ( Array, (!), listArray )
import Data.List ( scanl' )

main :: IO ()
main = interact (encode . solve . decode)

decode :: String -> [[Int]]
decode =  map (map read . words) . lines

encode :: [[Int]] -> String
encode = unlines . map (unwords . map show)

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [h,w]:rss -> case splitAt h rss of
        (xss, [q]:qss) -> map (query tab) qss
            where
                tab = accumTab (+) 0 (h,w) xss

query :: Array (Int,Int) Int -> [Int] -> [Int]
query tab [a,b,c,d] = [s]
    where
        s = tab ! (a-1,b-1) + tab ! (c,d)
          - tab ! (a-1,d) - tab ! (c,b-1)

accumTab :: (a -> a -> a) -> a -> (Int, Int)
         -> [[a]] -> Array (Int,Int) a
accumTab acc e (h,w)
    = listArray ((0,0),(h,w)) 
    . concat
    . scanl' (zipWith acc) (replicate (w+1) e)
    . map (scanl' acc e)
