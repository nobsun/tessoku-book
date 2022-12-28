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
    [n]:pas -> [[ maximum $ map fst $ msc n parr ]] where
        parr = listArray (1,n) $ map conv pas
        conv [p,a] = (p,a)

msc :: Int -> Array Int (Int, Int) -> [(Int, (Int, Int))]
msc n parr = iter [(0, (1,n))] where
    iter step@((_,(l,r)):_)
        | l == r    = step
        | otherwise = iter (splice max (map branch step))
    branch (c,(l,r)) = ((c1, (l+1,r)), (c2, (l,r-1))) where
        c1 = c + al'
        c2 = c + ar'
        (pl,al) = parr ! l
        (pr,ar) = parr ! r
        al' | l < pl && pl <= r = al
            | otherwise         = 0
        ar' | l <= pr && pr < r = ar
            | otherwise         = 0

splice :: (a -> a -> a) -> [(a,a)] -> [a]
splice op = foldr phi [] where
    phi (p,q) [] = [p,q]
    phi (p,q) (r:rs) = p : op q r : rs
