

module Spec where

import qualified Lib as P30
import Test.QuickCheck


main :: IO ()
main = do
    quickCheck (const False :: String -> Bool)
