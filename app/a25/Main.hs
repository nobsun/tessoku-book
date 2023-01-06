{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Control.Arrow
import Data.List

main :: IO ()
main = B.interact (encode . solve . decode)

solve :: [[Char]] -> [[Int]]
solve dss = case dss of
    hw:css -> case map read (words hw) of
        [h,w] -> [[countPath h w css]]

countPath :: Int -> Int -> [[Char]] -> Int
countPath h w = fst . foldl phi (0, row0)
    where
        row0 :: [Int]
        row0 = 1 : replicate (w-1) 0
        phi :: (Int, [Int]) -> [Char] -> (Int, [Int])
        phi (_,fs) = first fst . mapAccumL psi (0,fs) 
        psi (g,f:fs) '.' = ((fg,fs), fg) where fg = f + g
        psi (g,f:fs) '#' = ((0 ,fs), 0)

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
