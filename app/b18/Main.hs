module Main where

import Control.Arrow
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

encode :: [[Int]] -> B.ByteString
encode = B.unlines . map (B.unwords . map showInt)

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    [n,s]:as:_ -> sss s (zip as [1..])

sss :: Int -> [(Int, Int)] -> [[Int]]
sss s = iter [(0,[])]
    where
        iter _ [] = [[-1]]
        iter bs ((a,i):ais)
            = case dropWhile ((s <) . fst) (map ((a+)***(i:)) bs) of
                [] -> iter bs ais
                cs@((c,ps):_)
                    | c == s    -> [length ps] : [reverse ps]
                    | otherwise -> iter (cs >< bs) ais

(><) :: Ord a => [(a,[b])] -> [(a,[b])] -> [(a,[b])]
[] >< ys = ys
xs >< [] = xs
xxs@((x,is):xs) >< yys@((y,js):ys) = case compare x y of
    LT -> (y,js) : (xxs >< ys)
    EQ -> (x,is) : (xs >< ys)
    GT -> (x,is) : (xs >< yys)
