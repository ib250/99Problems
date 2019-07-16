

module P60Spec where

import qualified P60
import Test.QuickCheck


main :: IO ()
main = quickCheck (const False :: String -> Bool)
