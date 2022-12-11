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
    [n]:hs:_ -> [[ frog hs ]]

frog :: [Int] -> Int
frog hs = count (zipWith ((abs .) . subtract) hs (drop 1 hs))
                (zipWith ((abs .) . subtract) hs (drop 2 hs))

count :: [Int] -> [Int] -> Int
count (a:as) bs
    = iter 0 a as bs
    where
        iter _ q [] _ = q
        iter p q (a:as) (b:bs)
            = iter q (min (a+q) (b+p)) as bs

