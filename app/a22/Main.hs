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
    [n]:as:bs:_ -> [[da ! n]] where
        iab = zip3 [1 .. n] as bs
        ca = accumArray (flip (:)) [] (1,n)
           $ (1,0) : concat 
           [ [(a, d+100), (b, d+150)] 
           | (i,a,b) <- iab, let d = da ! i]
        da = maximum . (minBound :) <$> ca 
