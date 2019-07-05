module Spec where

import qualified Lib as Sol
import qualified Data.Set as Set
import Test.QuickCheck


lastSpec :: Eq a => NonEmptyList a -> Bool
lastSpec (NonEmpty xs) = last xs == Sol.myLast xs


butLastSpec :: Eq a => NonEmptyList a -> Bool
butLastSpec (NonEmpty xs)
    | length xs == 1 = True
    | otherwise = xs !! (length xs - 2) == Sol.myButLast xs


lengthSpec :: [a] -> Bool
lengthSpec xs = length xs == Sol.myLength xs


reverseSpec :: Eq a => [a] -> Bool
reverseSpec xs = reverse xs == Sol.myReverse xs


isPalindromeSpec :: Eq a => [a] -> Bool
isPalindromeSpec xs
    | Sol.isPalindrome xs = xs == reverse xs
    | otherwise = True


compressSpec :: (Ord a, Eq a) => [a] -> Bool
compressSpec xs =
    length compressed >= length setCompressed
    &&
    all (`elem` xs) compressed
    where compressed = Sol.compress xs
          setCompressed = Set.fromList xs


packSpec :: Eq a => [a] -> Bool
packSpec xs = (concat . Sol.pack) xs == xs


rleEncodeSpec :: Eq a => [a] -> Bool
rleEncodeSpec xs =
    xs == (Sol.rleDecode . Sol.rleEncode) xs
    
main :: IO ()
main = do
    quickCheck (lastSpec :: NonEmptyList Char -> Bool)
    quickCheck (butLastSpec :: NonEmptyList Char -> Bool)
    quickCheck (lengthSpec :: [Int] -> Bool)
    quickCheck (compressSpec :: String -> Bool)
    quickCheck (packSpec :: String -> Bool)
    quickCheck (rleEncodeSpec :: String -> Bool)
