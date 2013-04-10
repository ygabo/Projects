preparednessQuotient :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
preparednessQuotient i s c pn pa d n = (top pa s c) / (bottom pn d n i)

top :: Double -> Double -> Double -> Double
top pa s c = (8 * pa * (s + c))

bottom :: Double -> Double -> Double -> Double -> Double
bottom pn d n i 
	| d >= 1 && d <= 10 && n >= 1 && n <= 10 && i >= 1 && i <= 10 = (3 * pn * (d + n + i))
{-
kungPaoFactor :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
kungPaoFactor r dm ds n c ft ff s = (firstComponent n ds dm) + (secondComponent s r c ft ff)

firstComponent :: Int -> Int -> Int -> Int
firstComponent n ds dm = (n / 30) - (ds / dm)

secondComponent :: Int -> Int -> Int -> Int -> Int -> Int
secondComponent s r c ft ff = (numerator s r) / (denominator c ft ff)

numerator :: Int -> Int -> Ints
numerator s r = (10 * s * s * sqrt(r))

denominator :: Int -> Int -> Int -> Int
denominator c ft ff = c * (ft - ff + 1)
-}
multiply :: Integer -> Integer -> Integer
multiply a b
	| b == 0 || a == 0 = 0
	| b == 1 = a
	| a == 1 = b
	| otherwise = a + multiply a (b - 1)

multiply_tr :: Integer -> Integer -> Integer
multiply_tr a b = multiply_tr_helper a b a

multiply_tr_helper :: Integer -> Integer -> Integer -> Integer
multiply_tr_helper a b sum
	| b == 1 = sum
	| otherwise = multiply_tr_helper a (b - 1) (sum + a)

power :: Integer -> Integer -> Integer
power a b
	| b == 0 = 1
	| otherwise = multiply a (power a (b - 1))

power_tr :: Integer -> Integer -> Integer
power_tr a b = power_tr_helper a b 1

power_tr_helper :: Integer -> Integer -> Integer -> Integer
power_tr_helper a b product
	| a == 0 = 0
	| a == 1 = 1
	| b == 0 = product
	| otherwise = power_tr_helper a (b - 1) (multiply_tr product a)

harmonic :: Int -> Float
harmonic n
	| n == 1 = 1
	| otherwise = (1 / fromIntegral n) + (harmonic (n - 1))

harmonic_tr :: Int -> Float
harmonic_tr n = harmonic_tr_helper n 1

harmonic_tr_helper :: Int -> Float -> Float
harmonic_tr_helper n sum
	| n == 1 = sum
	| otherwise = harmonic_tr_helper (n - 1) (sum + ( 1 / fromIntegral n ))
