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

encode :: [[Double]] -> B.ByteString
encode = B.unlines . map (B.unwords . map showDouble)

showDouble :: Double -> B.ByteString
showDouble = B.pack . show

solve :: [[Int]] -> [[Double]]
solve dds = case dds of
    [n]:_ -> [[ search (fromIntegral n) ]]

search :: Double -> Double
search dn = iter 0 (10 ** 5)
    where
        iter l r
            | (r - l) / l < 0.001 = m
            | otherwise = case compare dn (m ** 3 + m) of
                LT -> iter l m
                EQ -> m
                GT -> iter m r
                where
                    m = (l + r) / 2
