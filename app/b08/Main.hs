module Main where

import qualified Data.ByteString.Char8 as B
import Data.Array ( Array, elems, (!), accumArray, listArray )
import Data.List ( scanl' )
import Data.Maybe

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
    [n]:rss -> case splitAt n rss of
        (ijs, [q]:qss) -> map (query tab) qss
            where
                tab = accumTab (+) 0 (h,w) pss
                pss = initTab (+) 0 (h,w) ijs'
                ijs' = map conv ijs
                conv [i,j] = ((i,j), 1)
                (is,js) = unzip (map fst ijs')
                h = maximum is
                w = maximum js

query :: Array (Int, Int) Int -> [Int] -> [Int]
query arr [a,b,c,d] = [s]
    where
        s = arr ! (a-1,b-1) + arr ! (c,d)
          - arr ! (a-1,d) - arr ! (c,b-1)

accumTab :: (a -> a -> a) -> a -> (Int, Int)
         -> [[a]] -> Array (Int, Int) a
accumTab acc e (h,w)
    = listArray ((0,0), (h,w))
    . concat
    . scanl' (zipWith acc) (replicate (w+1) e)
    . map (scanl' acc e)

initTab :: (a -> a -> a) -> a -> (Int, Int)
        -> [((Int, Int), a)] -> [[a]]
initTab acc e (h,w)
    = splitEvery w
    . elems
    . accumArray acc e ((1,1),(h,w))

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)
