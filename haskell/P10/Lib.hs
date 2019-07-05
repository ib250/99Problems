
module Lib where

import qualified Data.List as List
import qualified Data.Monoid as Monoid


myLast :: [a] -> a
myLast = head . reverse


myButLast :: [a] -> a
myButLast = last . init


elementAt :: [a] -> Int -> a
elementAt xs ix = xs !! (ix - 1)


myLength :: [a] -> Int
myLength = Monoid.getSum . foldMap (Monoid.Sum . (const 1))


myReverse :: [a] -> [a]
myReverse = foldr append' []
    where append' x xs = xs ++ [x]


isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = firstHalf == lastHalf
    where (n, r) = (length xs) `divMod` 2
          firstHalf = take n xs
          lastHalf = drop (n + r) xs


data NestedL a = Elem a
               | List [NestedL a]
               deriving (Eq, Show)


flatten :: NestedL a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs



newtype DupL a = DupL { unDup :: [a] } deriving (Eq, Show)

instance Eq a => Semigroup (DupL a) where
    (DupL []) <> rhs = rhs
    lhs <> (DupL []) = lhs
    (DupL lhs) <> (DupL rhs)
        | last lhs == head rhs = (DupL lhs) <> (DupL (tail rhs))
        | otherwise = (DupL (lhs <> [head rhs])) <> (DupL (tail rhs))


instance Eq a => Monoid (DupL a) where mempty = DupL []

compress :: Eq a => [a] -> [a]
compress = unDup . foldMap (DupL . pure)


-- TODO can i find a monoid which does this?
pack :: Eq a => [a] -> [[a]]
pack = foldr groupings []
    where groupings :: Eq a => a -> [[a]] -> [[a]]
          groupings x [] = [[x]]
          groupings x (y:ys)
                | x == head y = ((x:y):ys)
                | otherwise = [x]:y:ys


rleEncode :: Eq a => [a] -> [(Int, a)]
rleEncode xs = zip counts (map head $ ids)
    where ids = List.group xs
          counts = map length ids


rleDecode :: [(Int, a)] -> [a]
rleDecode = concatMap (uncurry replicate)
