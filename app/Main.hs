module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )

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
    _ -> undefined
