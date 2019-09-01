

module P60Spec where

import Numeric.Natural
import qualified P60
import Test.QuickCheck


balancedTreesSpec :: Positive Int -> Bool
balancedTreesSpec (Positive n) =
    all P60.isBalancedTree (take 10 trees)
    where trees = P60.balancedTrees (fromIntegral n) '\0'

main :: IO ()
main = quickCheck (balancedTreesSpec :: Positive Int -> Bool)
