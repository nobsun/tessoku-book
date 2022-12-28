{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List
import Data.Ord

main :: IO ()
main = B.interact (encode . solve . decode)

decode :: B.ByteString -> [[String]]
decode =  map (map B.unpack . B.words) . B.lines

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

encode :: [[Int]] -> B.ByteString
encode = B.unlines . map (B.unwords . map showInt)

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[String]] -> [[Int]]
solve dds = case dds of
    [s]:[t]:_ -> [[ mce s t ]]

mce :: String -> String -> Int
mce xs = head . foldr phi row0 where
    row0 = [n, n-1 .. 0] where
        n = length xs
    phi !y row = foldr step [ 1 + last row] xes where
        xes = zip3 xs row (tail row)
        step (!x,!es1,!es2) row = h : row where
            h | x == y    = es2
              | otherwise = 1 + minimum [es2, head row, es1]
