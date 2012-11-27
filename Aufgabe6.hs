module Aufgabe6 where

data Nat = Z | S Nat deriving (Show)

show :: Nat -> String
show n = (intToRoem.(\(_,x)->x).natToInt) (n, 0)

natToInt :: (Nat, Int) -> (Nat, Int)
natToInt (Z, a) = (Z, a)
natToInt ((S x), a) = natToInt(x, a+1)

intToRoem :: Int -> String
intToRoem 0 = ""
intToRoem a
	| a < 4     = repeatX 'I' a
	| a == 4    = "IV"
	| a < 9     = "V" ++ intToRoem (a-5)
	| a == 9    = "IX"
	| a < 40    = repeatX 'X' (div a 10) ++ intToRoem (mod a 10)
	| a < 50    = "XL" ++ intToRoem (mod a 10)
	| a < 90    = "L"  ++ intToRoem (mod a 50)
	| a < 99    = "XC" ++ intToRoem (mod a 90)
	| a == 99   = "IC"
	| a < 400   = repeatX 'C' (div a 100) ++ intToRoem (mod a 100)
	| a < 500   = "CD" ++ intToRoem (mod a 400)
	| a < 900   = "D"  ++ intToRoem (mod a 500)
	| a < 999   = "CM" ++ intToRoem (mod a 900)
	| a == 999  = "IM"
	| otherwise = repeatX 'M' (div a 1000) ++ intToRoem (mod a 1000)

repeatX :: a -> Int -> [a]
repeatX c num = foldr (:) [] (take num (repeat c))
