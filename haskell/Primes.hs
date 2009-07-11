--contains an infinite list of primes
module Primes where

cleanDivide :: Int -> Int -> Bool
cleanDivide x y = x == y * (x `div` y)

prime :: Int -> Int
prime 1 = 2
prime n = nextPrime n (primes !! (n-2))

nextPrime :: Int -> Int -> Int
nextPrime x y = if and $ map (not . (cleanDivide $ y+1)) (take (x-1) primes) then y+1 else nextPrime x (y+1)

primes :: [Int]
primes = map prime [1..]

difPrime :: Int -> Int
difPrime n = prime(n+1) - prime(n)
