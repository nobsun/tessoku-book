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
    [n,k]:as:_ -> [[ count k as ]]

count :: Int -> [Int] -> Int
count k as = case mapAccumL phi (0, tail as) as of
    (_, cs) -> sum cs
    where
        phi (c,rs) a = case span (close k a) rs of
            (ps, qs) -> ((d-1,qs), d)
                where
                    d = c + length ps

close :: Int -> Int -> Int -> Bool
close k a r = r - a <= k
