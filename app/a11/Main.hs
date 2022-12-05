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
    [n,x]:as:_ -> [[ searchIndex arr x ]]
        where
            arr = listArray (1,n) as

searchIndex :: Array Int Int -> Int -> Int
searchIndex arr x = iter (bounds arr) x
    where
        iter (l,r) x = case compare x (arr ! m) of
            LT -> iter (l,m-1) x
            EQ -> m
            GT -> iter (m+1,r) x
            where
                m = (l + r) `div` 2
