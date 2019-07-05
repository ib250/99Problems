
module Spec where

import qualified Lib as Sol
import qualified Data.List as List
import Test.QuickCheck


rleSpec :: Eq a => [a] -> Bool
rleSpec xs =
    xs == (Sol.rleDecode . Sol.rleEncode) xs
    &&
    xs == (Sol.rleDecode . Sol.rleEncode') xs
    &&
    Sol.rleEncode' xs == Sol.rleEncode xs


dupliSpec :: [a] -> Bool
dupliSpec xs = repliSpec xs (Positive 2)


repliSpec :: [a] -> Positive Int -> Bool
repliSpec xs (Positive n) =
    (length (Sol.repli xs n)) == (n * (length xs))


dropEverySpec :: Eq a => [a] -> Positive Int -> Bool
dropEverySpec xs (Positive n) =
    all (`elem` xs) ys && length ys <= length xs
    where ys = Sol.dropEvery xs n


splitSpec :: Eq a => [a] -> Int -> Bool
splitSpec xs n = xs == (lhs ++ rhs)
    where (lhs, rhs) = (Sol.split xs n)


data Slices a = Slices (NonEmptyList a) Int Int
                deriving (Eq, Show)


instance Arbitrary a => Arbitrary (Slices a) where
    arbitrary = do
        b <- arbitrary
        starts <- elements [1..(length' b)]
        ends <- elements [starts..(length' b)]
        return $ Slices b starts ends
        where length' = length . getNonEmpty


sliceSpec :: Eq a => Slices a -> Bool
sliceSpec (Slices xs i j) =
    (Sol.slice xs' i j) `List.isInfixOf` xs'
    where xs' = getNonEmpty xs


rotateSpec :: Eq a => [a] -> NonZero Int -> Bool
rotateSpec xs (NonZero n) =
    xs == Sol.rotate ys (-n)
    where (l, ys) = (length xs, Sol.rotate xs n)


data Removes a = Removes (NonEmptyList a) Int
                 deriving (Eq, Show)


instance Arbitrary a => Arbitrary (Removes a) where
    arbitrary = do
        xs <- arbitrary
        n  <- elements [1..(length . getNonEmpty) xs]
        return $ Removes xs n


removeAtSpec :: Eq a => Removes a -> Bool
removeAtSpec (Removes (NonEmpty xs) n) =
    xs == (take (n - 1) ys) ++ [x] ++ (drop (n - 1) ys)
    where (x, ys) = Sol.removeAt n xs
    

main :: IO ()
main = do
    quickCheck (rleSpec   :: String -> Bool)
    quickCheck (dupliSpec :: String -> Bool)
    quickCheck (repliSpec :: String -> Positive Int -> Bool)
    quickCheck (dropEverySpec :: String -> Positive Int -> Bool)
    quickCheck (splitSpec :: String -> Int -> Bool)
    quickCheck (sliceSpec :: Slices Char -> Bool)
    quickCheck (rotateSpec :: String -> NonZero Int -> Bool)
    quickCheck (removeAtSpec :: Removes Char -> Bool)
