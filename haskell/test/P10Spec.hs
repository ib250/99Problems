module P10Spec where

import qualified P10
import qualified Data.Set as Set
import Test.QuickCheck


lastSpec :: Eq a => NonEmptyList a -> Bool
lastSpec (NonEmpty xs) = last xs == P10.myLast xs


butLastSpec :: Eq a => NonEmptyList a -> Bool
butLastSpec (NonEmpty xs)
    | length xs == 1 = True
    | otherwise = xs !! (length xs - 2) == P10.myButLast xs


lengthSpec :: [a] -> Bool
lengthSpec xs = length xs == P10.myLength xs


reverseSpec :: Eq a => [a] -> Bool
reverseSpec xs = reverse xs == P10.myReverse xs


isPalindromeSpec :: Eq a => [a] -> Bool
isPalindromeSpec xs
    | P10.isPalindrome xs = xs == reverse xs
    | otherwise = True


compressSpec :: (Ord a, Eq a) => [a] -> Bool
compressSpec xs =
    length compressed >= length setCompressed
    &&
    all (`elem` xs) compressed
    where compressed = P10.compress xs
          setCompressed = Set.fromList xs


packSpec :: Eq a => [a] -> Bool
packSpec xs = (concat . P10.pack) xs == xs


rleEncodeSpec :: Eq a => [a] -> Bool
rleEncodeSpec xs =
    xs == (P10.rleDecode . P10.rleEncode) xs
    
main :: IO ()
main = do
    quickCheck (lastSpec :: NonEmptyList Char -> Bool)
    quickCheck (butLastSpec :: NonEmptyList Char -> Bool)
    quickCheck (lengthSpec :: [Int] -> Bool)
    quickCheck (compressSpec :: String -> Bool)
    quickCheck (packSpec :: String -> Bool)
    quickCheck (rleEncodeSpec :: String -> Bool)
