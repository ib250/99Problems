
module P70Spec where

import qualified P70
import Test.QuickCheck


main :: IO ()
main = quickCheck (const False :: String -> Bool)
