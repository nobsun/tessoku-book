module ScanMain where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array ( accumArray, elems )
import Data.List ( transpose, scanl' )

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

-- 

solve :: [[Int]] -> [[Int]]
solve dds = case dds of
    _ -> undefined

--

scanTab :: (a -> a -> a) -> a -> [[a]] -> [[a]]
scanTab acc e
    = twice (transpose . map (tail . scanl' acc e))

twice :: (a -> a) -> (a -> a)
twice f = f . f

type Rng = (Idx,Idx)
type Idx = (Int,Int)

initTab :: (a -> a -> a) -> a -> Rng -> [(Idx, a)] -> [[a]]
initTab acc e ((u,l),(d,r))
    = splitEvery (r-l+1)
    . elems
    . accumArray acc e ((u,l),(d,r))

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)
