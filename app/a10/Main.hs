module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array ( (!), listArray, Array )
import Data.List ( scanl' )

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

-- 

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [n]:as:[d]:lrs -> map (query ltab rtab) lrs
        where
            ltab = listArray (0,n) (scanl' max 0 as)
            rtab = listArray (1,n+1) (reverse (scanl' max 0 (reverse as)))

query :: Array Int Int -> Array Int Int -> [Int] -> [Int]
query ltab rtab [l,r]
    = [ max (ltab ! (l-1)) (rtab ! (r+1)) ]

