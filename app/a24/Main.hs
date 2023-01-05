module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List
import qualified Data.IntSet as S

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
    [n]:as:_ -> [[lis as]]

lis :: [Int] -> Int
lis = pred . S.size . foldl phi (S.singleton 0)
    where
        phi s a = case S.lookupLE a s of
            Just b
                | b < a -> S.insert a (psi s a)
            _           -> s
        psi s a = case S.lookupGT a s of
            Just c -> S.delete c s
            _      -> s


