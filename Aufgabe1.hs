module Main where
	-- fac: Calculation of Faktorielle: n!
	fac :: Integer -> Integer
	fac n
		| n > 0  = n * fac(n-1)
		| otherwise = 1

	-- binom: Like 49 over 6
	binom :: (Integer, Integer) -> Integer
	binom (n,k)
		| n >= 0 && k >= 0  = div (fac n) ((fac k)  * fac (n-k))
		| otherwise       = error "Binom error  "

	-- Catalan number for example 1
	katNumber :: Integer -> Integer
	katNumber n
		| n > 0 = div (binom ((2* (n-1)), n-1)) (n)
		| otherwise = error "Program error: Argument ungueltig"

	sumPowers :: Integer -> Integer -> Integer
	sumPowers n k
		| k < 0     = -1
		| otherwise = sum [i^k | i <- [1..n]]

	removeChar :: Char -> String -> String
	removeChar c input
	    | input == []     = ""
	    | c == head input = removeChar c (tail input)
	    | otherwise       = shrink c input

	shrink :: Char -> String -> String
	shrink c input
		| input == []     = ""
		| c == head input = c : removeChar c (tail input)
		| otherwise       = (head input) : shrink c (tail input)

	stretch :: Char -> Int -> String -> String
	stretch c i input
		| input == []     = ""
		| c == head input = (take i (repeat c)) ++ (stretch c i (tail input))
		| otherwise       = (head input) : (stretch c i (tail input))
