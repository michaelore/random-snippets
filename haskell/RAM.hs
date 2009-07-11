--Numerical integration
module RAM where

interval :: (Fractional a) => Int -> a -> a -> [a]
interval 0 _ b = [b]
interval n a b = a : (interval (n-1) (a+(b-a)/(fromIntegral n)) b)

midpoints :: (Fractional a) => [a] -> [a]
midpoints [] = []
midpoints [a] = []
midpoints (a:b:rest) = ((b-a)/2 + a) : (midpoints $ b:rest)

lRAM :: (Fractional a) => Int -> (a -> a) -> (a, a) -> a
lRAM n fun (a, b) = (b-a)*(sum $ map fun (init $ interval n a b))/(fromIntegral n)

mRAM :: (Fractional a) => Int -> (a -> a) -> (a, a) -> a
mRAM n fun (a, b) = (b-a)*(sum $ map fun (midpoints $ interval n a b))/(fromIntegral n)

rRAM :: (Fractional a) => Int -> (a -> a) -> (a, a) -> a
rRAM n fun (a, b) = (b-a)*(sum $ map fun (tail $ interval n a b))/(fromIntegral n)

trapezoid :: (Fractional a) => Int -> (a -> a) -> (a, a) -> a
trapezoid n fun range = ((lRAM n fun range) + (rRAM n fun range))/2.0

myfn :: (Floating a) => a -> a
myfn x = (sin x)/x
