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
    [n,k]:as:_ -> [[ count k (scanl' (+) 0 as) ]]

count :: Int -> [Int] -> Int
count k accs 
    = case mapAccumL phi (0, tail accs) (init accs) of
        (_,cs) -> sum cs
        where
            phi (c,[])         a = ((c-1,[]), c)
            phi (c,rrs@(r:rs)) a = case c of
                0 | r == a -> phi (c,rs) a
                _          -> case span within rrs of
                    (ps,qs) -> ((pred d, qs), d)
                        where
                            d = c + length ps
                where
                    within r = r - a <= k
