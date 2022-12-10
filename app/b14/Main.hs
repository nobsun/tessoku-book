{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List
import qualified Data.IntSet as Set

main :: IO ()
main = B.interact (encode . solve . decode)

decode :: B.ByteString -> [[Int]]
decode =  map (map readInt . B.words) . B.lines

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

encode :: [[B.ByteString]] -> B.ByteString
encode = B.unlines . map B.unwords

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[Int]] -> [[B.ByteString]]
solve dds = case dds of
    [n,k]:as:_ -> [[judge n k as]]

judge :: Int -> Int -> [Int] -> B.ByteString
judge n k as
    | any (`Set.member` ps) qs = "Yes"
    | otherwise                = "No"
    where
        ps = Set.fromList [ sum s | s <- subsequences bs]
        qs = [ k - sum s | s <- subsequences cs]
        (bs,cs) = splitAt (n `div` 2) as