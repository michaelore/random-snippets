--Module for dealing with uncertainty in calculations
--Example: (+) <$> makeUnc 0.05 5 <*> makeUnc 1 2
module Uncertainty where
import Control.Applicative

ordOfMag :: (Floating a, RealFrac a, Num b) => a -> b
ordOfMag n = fromIntegral $ floor $ log(n)/log(10)

makeUnc :: (Num a) => a -> a -> [a]
makeUnc unc n = [n + unc, n - unc]

uncSF :: (Floating a, RealFrac a, Integral b) => b -> a -> [a]
uncSF sf n = makeUnc (10 ^^ (ordOfMag(n)-sf)) n
