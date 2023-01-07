{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import qualified Data.IntSet as S
import Data.Bool
import Data.List hiding ( (\\) )

main :: IO ()
main = B.interact (encode . solve . decode)

solve :: [[Int]] -> [[Int]]
solve dss = case dss of
    [n]:_ -> map (:[]) $ takeWhile (n >=) primes

primes :: [Int]
primes = 2 : ([3 ..] \\ composites)
    where
        composites = mergeAll [ multiples p | p <- primes ]

multiples :: Int -> [Int]
multiples n = map (n *) [n ..]

(\\) :: [Int] -> [Int] -> [Int]
xxs@(x:xs) \\ yys@(y:ys) = case compare x y of
    LT -> x : (xs \\ yys)
    EQ -> xs \\ ys
    GT -> xxs \\ ys

mergeAll :: [[Int]] -> [Int]
mergeAll = foldr merge [] 
    where
        merge (x:xs) ys = x : merge' xs ys
        merge' xxs@(x:xs) yys@(y:ys) = case compare x y of
            LT -> x : merge' xs yys
            EQ -> x : merge' xs ys
            GT -> y : merge' xxs ys

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
