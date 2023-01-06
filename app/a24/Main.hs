{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List
import qualified Data.IntSet as S
import Debug.Trace

debug :: Bool
debug = True

tracing :: String -> a -> a
tracing msg
    | debug     = trace msg
    | otherwise = id

main :: IO ()
main = B.interact (encode . solve . decode)

solve :: [[Int]] -> [[Int]]
solve dss = case dss of
    [n]:as:_ -> [[lis as]]

lis :: [Int] -> Int
lis = pred . S.size . foldl phi (S.singleton 0)
    where
        phi s a = case S.lookupLE a s of
            Just b
                | b < a     
                -> tracing ("b <  a (="++show a++") : "++show s') s'
            _   -> tracing ("b == a (="++show a++") : "++show s ) s
            where s' = S.insert a (psi s a)
        psi s a = case S.lookupGT a s of
            Just c -> tracing ("    psi: delete "++show c) $ S.delete c s
            _      -> tracing "    psi: id s"            s

test :: [Int]
test = [3,1,4,1,5,9,2,6,5,3,5,8,9,3]

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
