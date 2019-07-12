-- {-# Language ScopedTypeVariables #-}

module Lib where


import qualified Data.List as List
import Control.Monad
import System.Random


insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = foldr alg [] xs
    where alg y rest | length rest == n = x:rest
                     | otherwise = y:rest


range :: Int -> Int -> [Int]
range x y = [x..y]


rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    ixs <- generate_ getStdGen
    return $ take n [xs !! ix | ix <-  List.nub ixs]
    where generate_ :: IO StdGen -> IO [Int]
          generate_ = fmap $ randomRs (0, length xs - 1)


rndLotto :: Int -> Int -> IO [Int]
rndLotto x y = take x <$> possibilities
    where possibilities :: IO [Int]
          possibilities = randomRs (1, y) <$> getStdGen


rndPermu :: [a] -> IO [a]
rndPermu xs = do
    perm_ <- genPermutation (length xs) getStdGen
    return $ fmap (xs!!) perm_
    where genPermutation :: Int -> IO StdGen -> IO [Int]
          genPermutation sz = fmap (take sz . List.nub . randomRs (0, sz - 1))


combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _  = [[]]
combinations 1 xs = fmap pure xs
combinations n (x:xs) = leftSubset ++ rightSubset
    where rightSubset = combinations n xs
          leftSubset  = [x:xs_ | xs_ <- combinations (n - 1) xs]


-- the list brackets get silly
type Solution a = [[a]]

-- can do without Eq ?
groupXs :: Eq a => [Int] -> [a] -> [Solution a]
groupXs ns xs = filter isDisjointParts solutions
    where solutions = sequenceA [f xs | f <- combinations <$> ns]
          isDisjointParts xs = sum ns == length (List.nub (mconcat xs))


lsort :: [[a]] -> [[a]]
lsort = List.sortBy (\x y -> compare (length x) (length y))


lfsort :: [[a]] -> [[a]]
lfsort = mconcat . List.sortOn length . alg
    where alg = List.groupBy (\x y -> length x == length y) . lsort
