
module P70 where


import Numeric.Natural
import qualified Data.List as List
import P60 (Tree(..), leaf, depth)


countLeaves :: Tree a -> Natural
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ lhs rhs) =
    countLeaves lhs + countLeaves rhs


collectLeaves :: Tree a -> [a]
collectLeaves Empty = []
collectLeaves (Branch x Empty Empty) = [x]
collectLeaves (Branch _ lhs rhs) =
    collectLeaves lhs <> collectLeaves rhs


treeInternals :: Tree a -> [a]
treeInternals Empty = []
treeInternals (Branch _ Empty Empty) = []
treeInternals (Branch x lhs rhs) =
    [x] <> treeInternals lhs <> treeInternals rhs


atLevel :: Natural -> Tree a -> [a]
atLevel _ Empty = []
atLevel 0 _ = [] -- should never happen
atLevel 1 (Branch x _ _) = [x]
atLevel n (Branch _ lhs rhs) =
    atLevel (n - 1) lhs <> atLevel (n - 1) rhs



-- TODOs ...

completeBinaryTree :: Natural -> a -> Tree a
completeBinaryTree 0 _ = Empty
completeBinaryTree 1 x = leaf x
completeBinaryTree n x = undefined
    


layout :: Tree a -> Tree (a, (Natural, Natural))
layout = undefined


stringToTree :: Monad m => String -> m (Tree Char)
stringToTree = undefined


treeToInorder, treeToPreorder :: Tree Char -> String
treeToInorder  = undefined
treeToPreorder = undefined


preInTree :: String -> String -> Tree Char
preInTree = undefined


dotStringTree :: Tree a -> String
dotStringTree = undefined
