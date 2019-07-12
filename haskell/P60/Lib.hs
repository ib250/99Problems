
module Lib where


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
balancedTrees 0 x = [Empty]
balancedTrees 1 x = [leaf x]
balancedTrees n x =
    let
        l = (n - 1) `div` 2
        r = (n - 1) - l
    in trees_ l r <> trees_ r l
    where trees_ i j = [ Branch x lhs rhs
                       | lhs <- balancedTrees i x
                       , rhs <- balancedTrees j x
                       ]


symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ lhs rhs) = depth lhs == depth rhs
