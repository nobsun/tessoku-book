{-# LANGUAGE OverloadedStrings #-}
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

encode :: [[B.ByteString]] -> B.ByteString
encode = B.unlines . map B.unwords

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[Int]] -> [[B.ByteString]]
solve dds = case dds of
    [n,s]:as:_ -> [[judge s as]]

judge :: Int -> [Int] -> B.ByteString
judge s as
    | check s as = "Yes"
    | otherwise  = "No"

check :: Int -> [Int] -> Bool
check s = iter [0]
    where
        iter _  [] = False
        iter bs (a:as) = case dropWhile (s <) (map (a +) bs) of
            [] -> iter bs as
            cs@(c:_)
                | c == s    -> True
                | otherwise -> iter (cs >< bs) as

(><) :: Ord a => [a] -> [a] -> [a]
[] >< ys = ys
xs >< [] = xs
xxs@(x:xs) >< yys@(y:ys) = case compare x y of
    LT -> y : (xxs >< ys)
    EQ -> x : (xs >< ys)
    GT -> x : (xs >< yys)
