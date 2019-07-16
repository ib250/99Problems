module P40 where


import qualified Data.List as List
import Control.Arrow ((&&&))

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime xs | even xs = False
           | otherwise = check_ xs [3..(xs - 2)]
    where check_ xs [] = True
          check_ xs (x:rest)
                | xs `mod` x == 0 = False
                | otherwise = check_ xs [e | e <- rest, e `mod` x /= 0]



euclideanGCD :: Integral a => a -> a -> a
euclideanGCD x 0 = x
euclideanGCD x y
    | x < y = euclideanGCD y x
    | otherwise = euclideanGCD y (x `mod` y)


coPrime :: Integral a => a -> a -> Bool
coPrime x y = gcd x y == 1


eulerTotient :: Integral a => a -> Int
eulerTotient m = length . filter (coPrime m) $ [1..(m - 1)]


primeFactors :: Integral a => a -> [a]
primeFactors m = concatMap replicate_ pFactors
    where pFactors = filter isPrime [2..m]
          replicate_ x = replicate (x `powerOf` m) x
          powerOf x n | n `mod` x == 0 = 1 + powerOf x (n `div` x)
                      | otherwise      = 0


primeFactorisation :: Integral a => a -> [(a, Int)]
primeFactorisation =
    map (head &&& length) . List.group . primeFactors


eulerTotient' :: Integral a => a -> a
eulerTotient' = totient' . primeFactorisation
    where totient' ps = product [(p - 1)*p^(r - 1) | (p, r) <- ps]


primesR :: Integral a => a -> a -> [a]
primesR x y = filter isPrime [x..y]


-- goldback's conjecture:
-- every even number can be written as the sum of two primes
-- every odd number can be written as the sum of 3 primes
-- the odd case follows if the even case is true
-- if x is odd, the result is (a, b) can be further expanded to (a, b, c)
goldback :: Integral a => a -> (a, a)
goldback x =
    head $ filter (\(n, m) -> isPrime n && isPrime m) solutions
    where ps = primesR 2 x
          solutions = map goldback_ ps
          goldback_ p = if x <= p then (x, p - x) else (p, x - p)


goldbackList :: Integral a => a -> a -> [(a, a)]
goldbackList x y = map goldback candidates
    where candidates = filter even [x..y]


goldbackList' :: Integral a => a -> a -> a -> [(a, a)]
goldbackList' x y z = filter ((>=z) . fst) $ goldbackList x y
