module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List

main :: IO ()
main = B.interact (encode . solve . decode)

decode :: B.ByteString -> [[Int]]
decode =  map (map readInt . B.words) . B.lines

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

encode :: [[Int]] -> B.ByteString
encode = B.unlines . map (B.unwords . map showInt)

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [n,k]:as:_ -> [[ search as k ]]

search :: [Int] -> Int -> Int
search as = iter (1,10^9)
    where
        iter (l,r) k = case compare k (count m) of
            GT | count r < k -> r + 1
               | otherwise   -> iter (m + 1, r) k
            _  | k < count l -> l
               | otherwise   -> iter (l, m - 1) k
            where
                m = (l + r) `div` 2
        count i = sum [ i `div` a | a <- as ]
