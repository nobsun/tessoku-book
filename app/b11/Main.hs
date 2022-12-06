module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array ( (!), bounds, listArray, Array )
import Data.List ( sort )

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
    [n]:as:[q]:qss -> map (query arr) qss
        where
            arr = listArray (0,n) (0:sort as)

query :: Array Int Int -> [Int] -> [Int]
query arr [x] = [searchIndex arr x]

searchIndex :: Array Int Int -> Int -> Int
searchIndex arr = iter (bounds arr)
    where
        iter (l,r) x 
            | x < arr ! l = l - 1
            | arr ! r < x = r
            | otherwise   = case compare x (arr ! m) of
                GT -> iter (m+1,r) x
                _  -> iter (l,m-1) x
                where
                    m = (l + r) `div` 2
