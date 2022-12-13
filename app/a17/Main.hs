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
    [n]:as:bs:_ -> minpath as bs

minpath :: [Int] -> [Int] -> [[Int]]
minpath (a:as) bs
    = iter (0,[1]) (a,[2,1]) (zip as [3..]) (zip bs [3..])
    where
        iter _ (_,qs) [] _ = [length qs] : [reverse qs]
        iter (c,ps) (d,qs) ((a,i):ais) ((b,j):bjs)
            = iter (d,qs) (e,rs) ais bjs
            where
                (e,rs) = if d+a <= c+b
                    then (d+a, i:qs)
                    else (c+b, j:ps)
