

module P30Spec where

import qualified Data.List as List
import qualified P30
import Test.QuickCheck



data Insertion a = Insertion a [a] Int
                   deriving (Eq, Show)


instance Arbitrary a => Arbitrary (Insertion a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        ix <- elements [1..(length' xs)]
        return $ Insertion x (getNonEmpty xs) ix
        where length' = length . getNonEmpty



insertAtSpec :: Eq a => Insertion a -> Bool
insertAtSpec (Insertion x xs n) =
    (ys !! (n - 1) == x) && (length ys >= length xs)
    where ys = P30.insertAt x xs n


data Combinations a = Combinations Int [a]
                      deriving (Eq, Show)


instance Arbitrary a => Arbitrary (Combinations a) where
    arbitrary = do
        xs <- arbitrary
        (Positive n) <- arbitrary :: Gen (Positive Int)
        return $ Combinations n xs


combinationsSpec :: Eq a => Combinations a -> Bool
combinationsSpec (Combinations n xs) =
    all isComb $ take 5 combs_
    where combs_ = P30.combinations n xs
          isComb ys = length ys == n && all (`elem`xs) (take 5 ys)



main :: IO ()
main = do
    quickCheck (insertAtSpec :: Insertion Int -> Bool)
    quickCheck (combinationsSpec :: Combinations Char -> Bool)
