module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array
import Data.List
import Data.Ord

main :: IO ()
main = B.interact (encode . solve . decode)

decode :: B.ByteString -> [[String]]
decode =  map (map B.unpack . B.words) . B.lines

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

encode :: [[Int]] -> B.ByteString
encode = B.unlines . map (B.unwords . map showInt)

showInt :: Int -> B.ByteString
showInt = B.pack . show

solve :: [[String]] -> [[Int]]
solve dds = case dds of
    [s]:[t]:_ -> [[ cost $ mce s t ]]

mce :: String -> String -> CostedOps
mce xs = head . foldr phi row0 where
    row0 = map ((,) <*> length) $ tails $ map Del xs
    phi y row = foldr step [Ins y `cons` last row] xes where
        xes = zip3 xs row (tail row)
        step (x,es1,es2) row = h : row where
            h | x == y    = Cpy x `cons` es2
              | otherwise = minimumBy (comparing cost)
                          [ Rep x y `cons` es2
                          , Del x `cons` head row
                          , Ins y `cons` es1 ]

data Op
    = Cpy Char
    | Del Char
    | Rep Char Char
    | Ins Char
    deriving (Eq, Show)

type CostedOps = ([Op], Int)

cost :: CostedOps -> Int
cost = snd

nil :: CostedOps
nil = ([], 0)

cons :: Op -> CostedOps -> CostedOps
cons o (os, c) = case o of
    Cpy _ -> (o:os, c)
    _     -> (o:os, 1 + c)
