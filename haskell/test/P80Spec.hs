
module P80Spec where

import qualified P80
import Test.QuickCheck


main :: IO ()
main = quickCheck (const False :: String -> Bool)
