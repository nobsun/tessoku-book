module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List

main :: IO ()
main = B.interact (encode . solve . decode)

decode :: B.ByteString -> [[String]]
decode =  map (map B.unpack . B.words) . B.lines

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

encode :: [[Int]] -> B.ByteString
encode = B.unlines . map (B.unwords . map showInt)

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[String]] -> [[Int]]
solve dds = case dds of
    _:[s]:_ -> [[ size $ lcs s (reverse s) ]]

lcs :: Eq a => [a] -> [a] -> Sized a
lcs s t = head $ foldr phi (replicate (length s + 1) nil) t
    where
        phi y row = foldr (step y) [nil] (zip3 s row (tail row))
        step y (x,cs1,cs2) row
            | x == y     = (x `cons` cs2) : row
            | otherwise  = cs1 `longer` head row : row

type Sized a = ([a], Int)

size :: Sized a -> Int
size = snd

nil :: Sized a
nil = ([], 0)

cons :: a -> Sized a -> Sized a
cons x (xs, n) = (x : xs, n + 1)

longer :: Sized a -> Sized a -> Sized a
longer xs@(_,m) ys@(_,n)
    | m < n     = ys
    | otherwise = xs


