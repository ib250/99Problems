module P60 where

import Numeric.Natural

data Tree a = Empty
            | Branch a (Tree a) (Tree a)
            deriving (Eq, Show)


leaf :: a -> Tree a
leaf x = Branch x Empty Empty


depth :: Tree a -> Natural
depth Empty = 0
depth (Branch _ lhs rhs) = 1 + max (depth lhs) (depth rhs)


nElems :: Tree a -> Natural
nElems Empty = 0
nElems (Branch _ lhs rhs) = 1 + nElems lhs + nElems rhs


balancedTrees :: Natural -> a -> [Tree a]
balancedTrees 0 _ = [Empty]
balancedTrees 1 x = [leaf x]
balancedTrees n x =
    let
        (l, r) = ((n - 1) `div` 2, (n - 1) - l)
    in trees_ l r <> trees_ r l
    where trees_ i j = [ Branch x lhs rhs
                       | lhs <- balancedTrees i x
                       , rhs <- balancedTrees j x
                       ]


isBalancedTree :: Tree a -> Bool
isBalancedTree Empty = True
isBalancedTree (Branch _ lhs rhs) =
    dl == dr || dr + 1 == dl || dl + 1 == dr
    where (dl, dr) = (depth lhs, depth rhs)


symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ lhs rhs) = depth lhs == depth rhs


construct :: Ord a => [a] -> Tree a
construct = foldl alg Empty
    where alg :: Ord a => Tree a -> a -> Tree a
          alg Empty x = leaf x
          alg (Branch y lhs rhs) x
                | x < y = Branch y (alg lhs x) rhs
                | otherwise = Branch y lhs (alg rhs x)


symmetricBalancedTrees :: Natural -> a -> [Tree a]
symmetricBalancedTrees = curry $ filter symmetric . uncurry balancedTrees


heightBalancedTrees :: Natural -> a -> [Tree a]
heightBalancedTrees 0 _ = [Empty]
heightBalancedTrees 1 x = [leaf x]
heightBalancedTrees n x =
    let
        (l, r) = (n - 1, n - 2)
    in trees_ l r <> trees_ r l
    where trees_ i j = [ Branch x lhs rhs
                       | lhs <- heightBalancedTrees i x
                       , rhs <- heightBalancedTrees j x
                       ]


heightBalancedTreeNodes :: Natural -> a -> [Tree a]
heightBalancedTreeNodes 0 _ = [Empty]
heightBalancedTreeNodes 1 x = [leaf x]
heightBalancedTreeNodes n x = undefined -- TODO
