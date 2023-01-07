{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.Function
import Data.List
import Data.Ord
import qualified Data.IntSet as S

main :: IO ()
main = B.interact (encode . solve . decode)

solve :: [[Int]] -> [[Int]]
solve dss = case dss of
    [n]:xys -> [[lis as]]
        where
            as = map snd
               $ sortBy cmp
               $ map conv xys
            conv [x,y] = (x,y)
            cmp (x,y) (x',y') = case compare x x' of
                EQ -> compare y' y
                o  -> o 

lis :: [Int] -> Int
lis = S.size . foldl phi S.empty
    where
        phi s a = S.insert a 
                $ maybe s (`S.delete` s) (S.lookupGE a s)

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
