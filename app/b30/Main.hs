{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List

main :: IO ()
main = B.interact (encode . solve . decode)

solve :: [[Int]] -> [[Int]]
solve dss = case dss of
    [h,w]:_ -> [[ comb (h+w-2) (min h w - 1) ]]

comb :: Int -> Int -> Int
comb n r = prodMod [n-(r-1) .. n] /% prodMod [1 .. r]

prodMod :: [Int] -> Int
prodMod = foldl' (*%) 1

power :: Int -> Int -> Int
power a b = case b of
    0 -> 1
    _ -> case b `divMod` 2 of
        (q, 0) -> square (power a q)
        (q, _) -> a *% square (power a q)

square :: Int -> Int
square x = x *% x

(*%) :: Int -> Int -> Int
m *% n = m * n `mod` base

(/%) :: Int -> Int -> Int
m /% n = m *% recipMod n

recipMod :: Int -> Int
recipMod a = power a (base - 2)

base :: Int
base = 10^9 + 7

--

class InterfaceForOJS a where
    readB :: B.ByteString -> a
    readBs :: B.ByteString -> [a]
    readBs = map readB . B.words
    decode :: B.ByteString -> [[a]]
    decode = map readBs . B.lines

    showB :: a -> B.ByteString
    showBs :: [a] -> B.ByteString
    showBs = B.unwords . map showB
    encode :: [[a]] -> B.ByteString
    encode = B.unlines . map showBs

instance InterfaceForOJS Int where
    readB = readInt
    showB = showInt

instance InterfaceForOJS String where
    readB = readStr
    showB = showStr

instance InterfaceForOJS Double where
    readB = readDbl
    showB = showDbl

instance InterfaceForOJS Char where
    readB = B.head
    showB = B.singleton
    readBs = B.unpack
    showBs = B.pack

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

showInt :: Int -> B.ByteString
showInt = B.pack . show

readStr :: B.ByteString -> String
readStr = B.unpack

showStr :: String -> B.ByteString
showStr = B.pack

readDbl :: B.ByteString -> Double
readDbl = read . B.unpack

showDbl :: Double -> B.ByteString
showDbl = B.pack . show
