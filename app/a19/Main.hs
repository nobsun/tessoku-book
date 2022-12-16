module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.List ( sort )

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
    [n,w]:wvs -> [[knapsack w (map conv wvs)]]
        where
            conv [w,v] = (w,v)

knapsack :: Int -> [(Int,Int)] -> Int
knapsack w wvs = iter [(0,0)] (sort wvs)
    where
        iter acc [] = maximum (map snd acc)
        iter acc (wv:rs)
            = case dropWhile ((w <) . fst) (map (add wv) acc) of
                []   -> maximum (map snd acc)
                acc' -> iter (acc' >< acc) rs
        add (x,y) (x',y') = (x+x', y+y')

(><) :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
[] >< ys = ys
xs >< [] = xs
xxs@(x@(wx,vx):xs) >< yys@(y@(wy,vy):ys)
    = case compare wx wy of
        GT -> case compare vx vy of
            GT -> x : (xs >< yys)
            _  -> xs >< yys
        EQ -> case compare vx vy of
            LT -> y : (xs >< ys)
            _  -> x : (xs >< ys)
        LT -> case compare vx vy of
            LT -> y : (xxs >< ys)
            _  -> xxs >< ys
