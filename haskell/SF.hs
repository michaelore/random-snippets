--Module for dealing with significant figures.
--Example: roundToSF $ SF 2 550 * SF 3 2.05
module SF where

data (Integral a, Floating b, RealFrac b) => SF a b = SF a b
    deriving (Show, Eq)

ordOfMag n = floor $ log(n)/log(10)

sfToSD sf n = ordOfMag n - sf

sdToSF d n = ordOfMag n - d

guessIntSF n = ordOfMag (fromInteger n) + 1

guessRatSF n = abs(ordOfMag (fromRational n)) + 1

roundToSF (SF sf n) = (10 ^^ (sfToSD sf n + 1)) * (fromInteger $ round $ n / (10 ^^ (sfToSD sf n + 1)))

instance (Integral a, Floating b, RealFrac b) => Num (SF a b) where
    (SF sf1 n1) * (SF sf2 n2) = SF (min sf1 sf2) (n1*n2)
    (SF sf1 n1) + (SF sf2 n2) = SF (sdToSF (max (sfToSD sf1 n1) (sfToSD sf2 n2)) (n1+n2)) (n1+n2)
    x - y = x + (negate y)
    negate (SF sf n) = (SF sf (negate n))
    abs (SF sf n) = (SF sf (abs n))
    signum (SF sf n) = (SF sf (signum n))
    fromInteger n = SF (guessIntSF n) (fromInteger n)

instance (Integral a, Floating b, RealFrac b) => Ord (SF a b) where
    compare (SF _ n1) (SF _ n2) = compare n1 n2

instance (Integral a, Floating b, RealFrac b) => Real (SF a b) where
    toRational (SF _ n) = toRational n

instance (Integral a, Floating b, RealFrac b) => Fractional (SF a b) where
    x / y = x * (recip y)
    recip (SF sf n) = (SF sf (recip n))
    fromRational n = SF (guessRatSF n) (fromRational n)
