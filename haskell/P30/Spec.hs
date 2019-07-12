

module Spec where

import qualified Lib as P30
import Test.QuickCheck


main :: IO ()
main = quickCheck (const False :: String -> Bool)
