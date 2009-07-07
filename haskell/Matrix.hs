--NOTE: This module may not work with non-ghc compilers/interpreters.
{-# LANGUAGE TypeSynonymInstances #-}
module Matrix where

import Data.List

deleteAt :: Int -> [a] -> [a]
deleteAt n xs = (take n xs) ++ (drop (n+1) xs)

type Matrix a = [[a]] --each interior list is a row

instance Num a => Num (Matrix a) where
	(+) = zipWith (zipWith (+))
	(-) = zipWith (zipWith (-))
	m1 * m2 = let t = transpose m2
		  in map (\row -> map (\column -> sum $ zipWith (*) row column) t) m1
	negate = map (map negate)
	abs = map (map abs)
	signum = map (map signum)
	fromInteger n = [[fromInteger n]]

minor :: (Num a) => Matrix a -> (Int, Int) -> a
minor m (x+1, y+1) = det $ map (deleteAt x) (deleteAt y m)

cofactor :: (Num a) => Matrix a -> (Int, Int) -> a
cofactor m (x, y) = if even $ x+y
			then minor m (x, y)
			else -(minor m (x, y))

det :: (Num a) => Matrix a -> a
det [[n]] = n
det [[a, b], [c, d]] = a*d-b*c
det m = sum $ zipWith (*) (head m) (map (\x -> cofactor m (x, 1)) [1..])

invert :: (Fractional a) => Matrix a -> Matrix a
invert m = let len = length m
	       idet = 1 / (det m)
	   in if (det m) == 0
	      then undefined
	      else map (\x -> map (\y -> cofactor m (x, y) * idet) [1..len]) [1..len]
