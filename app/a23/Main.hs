module Main where

import Control.Arrow
import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.Bits
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
solve dss = case dss of
    [n,m]:bss -> [[mnc m n (map bin2int bss)]]

mnc :: Int -> Int -> [Int] -> Int
mnc m n as
    | coupons > m = -1 
    | otherwise   = coupons 
    where
        coupons = foldl' phi arr0 as ! ub
        ub   = 2^n - 1
        imp  = m + 1
        arr0 = listArray (0, ub) $ 0 : replicate ub imp
        phi arr a
            = accumArray min imp (0, ub)
            [ next prev | prev <- assocs arr
                        , next <- [id, (a .|.) *** (1 +)]]

bin2int :: [Int] -> Int
bin2int = foldl' ((+) . (2 *)) 0
