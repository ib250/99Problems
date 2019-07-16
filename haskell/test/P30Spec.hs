

module P30Spec where

import qualified P30
import Test.QuickCheck


main :: IO ()
main = quickCheck (const False :: String -> Bool)
