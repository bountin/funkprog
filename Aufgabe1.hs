module Main where
	fac :: Integer -> Integer
	fac n
		| n > 0  = n * fac(n-1)
		| otherwise = 1

	binom :: (Integer, Integer) -> Integer
	binom (n,k)
		| n > 0 && k > 0  = div (fac n) ((fac k)  * fac (n-k))
		| otherwise       = error "Binom error  "

	katNumber :: Integer -> Integer
	katNumber n
		| n > 0 = div (binom ((2*n), n)) (n+1)
		| otherwise = error "Program error: Argument ungueltig"
