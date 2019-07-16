
module P20 where

import qualified Data.List as List


data Encoding a = Single a
                | Multiple Int a
                deriving (Eq, Show)


rleEncode :: Eq a => [a] -> [Encoding a]
rleEncode = map makeEncoding . List.group
    where makeEncoding [x] = Single x
          makeEncoding xs@(x:_)  = Multiple (length xs) x


rleDecode :: [Encoding a] -> [a]
rleDecode = concatMap expandEncodeing
    where expandEncodeing (Single x) = [x]
          expandEncodeing (Multiple i x) = replicate i x


rleEncode' :: Eq a => [a] -> [Encoding a]
rleEncode' = foldr alg []
    where alg :: Eq a => a -> [Encoding a] -> [Encoding a]
          alg x [] = [Single x]
          alg x xs@(Single c:rest)
                | x == c = Multiple 2 x:rest
                | otherwise = Single x:xs
          alg x xs@(Multiple n c:rest)
                | x == c = Multiple (n + 1) x:rest
                | otherwise = Single x:xs


dupli :: [a] -> [a]
dupli = concatMap (replicate 2)


repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs


dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
    [e | (ix, e) <- zip [1..] xs , ix `mod` n /= 0]


split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)


slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b - a + 1) $ drop (a - 1) xs


rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0 = drop n xs ++ take n xs
            | otherwise = reverse $ rotate (reverse xs) (-n)


removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1),  take (n - 1) xs ++ drop n xs)
