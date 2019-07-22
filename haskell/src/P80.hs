{-# Language DeriveFunctor #-}

module P80 where


import Numeric.Natural


data MultiWayTree a = Node a [MultiWayTree a]
                      deriving (Eq, Show, Functor)


nNodes :: MultiWayTree a -> Natural
nNodes (Node a xs) = 1 + sum (fmap nNodes xs)
