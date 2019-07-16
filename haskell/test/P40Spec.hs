

module P40Spec where

import qualified P40
import Test.QuickCheck


main :: IO ()
main = quickCheck (const False :: String -> Bool)
