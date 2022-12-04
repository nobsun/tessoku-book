module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import Data.Array ( accumArray, elems )
import Data.List ( foldl', transpose, scanl' )

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
    [n]:abcds -> [[a]]
        where
            a = sum
              $ map count
              $ scanTab (+) 0
              $ initTab (+) 0 ((u,l), (d,r))
              $ preprocess abcds
            [u,l] = map minimum (take 2 asbscsds)
            [d,r] = map maximum (drop 2 asbscsds)
            asbscsds = transpose abcds

count :: [Int] -> Int
count = foldl' phi 0
    where
        phi a 0 = a
        phi a b = a + 1

preprocess :: [[Int]] -> [(Idx, Int)]
preprocess = concatMap conv 

conv :: [Int] -> [(Idx, Int)]
conv [a,b,c,d]
    = [((a,b), 1), ((a,d), -1), ((c,b), -1), ((c,d), 1)]

--
{-
(a,b)       (a,d)

(c,b)       (c,d)
-}
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
