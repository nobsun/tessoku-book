{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.Bits


main :: IO ()
main = B.interact (encode . solve . decode)

solve :: [[Int]] -> [[Double]]
solve dss = case dss of
    [n]:xys -> [[tsp n xya]] where
        xya = listArray (1,n) (map conv xys)
        conv [x,y] = (fromIntegral x, fromIntegral y)

type Coord = (Double, Double)
tsp :: Int -> Array Int Coord -> Double
tsp n xya
    = minimum [ travela ! ub ! c + dist 1 c
              | c <- [2 .. n]]
    where
        ub = 2^n - 1
        dist i j = distance (xya ! i) (xya ! j)
        travela = listArray (1,ub)
                [ listArray (1,n) [ travel s c 
                                  | c <- [1 .. n] 
                                  ]
                | s <- [1 .. ub] 
                ]
        travel s c
            | not (1 ∈ s)      = infinity
            | not (c ∈ s)      = infinity
            | s == singleton 1 = 0
            | isDoubleton s    = dist 1 c
            | otherwise        = minimum
                               [ travela ! t ! d + dist c d
                               | d <- [2 .. n], d ∈ t]
                where
                    t = s \\ singleton c 

type BitRep = Int
type Set  = BitRep
type Elem = BitRep

(∈) :: Elem -> Set -> Bool
e ∈ s = testBit s (pred e)

(\\) :: Set -> Set -> Set
s \\ t = s .&. complement t

singleton :: Elem -> Set
singleton = setBit 0 . pred

isDoubleton :: Set -> Bool
isDoubleton = (2 ==) . popCount

infinity :: Double
infinity = 1 / 0

distance :: Coord -> Coord -> Double
distance (x1,y1) (x2,y2)
    = sqrt $ (x1 - x2)^2 + (y1 - y2)^2


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
