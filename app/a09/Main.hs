module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array ( elems, accumArray )
import Data.List ( transpose, scanl' )

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
    [h,w,n]:abcds -> scanTab (+) 0
                   $ initTab (+) 0 (h,w)
                   $ concatMap (conv (h,w)) abcds

scanTab :: (a -> a -> a) -> a -> [[a]] -> [[a]]
scanTab acc e
    = twice (transpose . map (tail . scanl' acc e))

twice :: (a -> a) -> (a -> a)
twice f = f . f

initTab :: (a -> a -> a) -> a -> (Int, Int)
        -> [((Int, Int), a)] -> [[a]]
initTab acc e (h,w)
    = splitEvery w
    . elems
    . accumArray acc e ((1,1),(h,w))

splitEvery :: Int -> [a] -> [[a]] 
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

conv :: (Int, Int) -> [Int] -> [((Int, Int), Int)]
conv (h,w) [a,b,c,d]
    = [ ijk | ijk@((i,j),k) <- fcs, i <= h, j <= w ]
    where
        fcs = [((a,b),1),((c+1,d+1),1)
              ,((a,d+1),-1),((c+1,b),-1)]

{-
(a,b)       (a,d) (a,d+1)

(c,b)       (c,d)
(c+1,b)           (c+1,d+1)
-}
