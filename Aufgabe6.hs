module Aufgabe6 where

data Nat = Z | S Nat

instance Show Nat where
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

{-------------------------------------
 --- Second part of the assignment ---
 -------------------------------------}

data RatNumbers = Rat Numerator Denominator
type Numerator = Nat
type Denominator = Nat
newtype NatP = NP (Nat,Nat) deriving Show
class Nf a where
	t2nf :: a -> a

{- Negabinary Definition and functions from Aufgabe{2,3,4} -}

euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (mod a b)

type NegaBinary = String

dropZeroes :: NegaBinary -> NegaBinary
dropZeroes input = if (dropped == "") then "0" else dropped
      where dropped = realDropZeroes input

realDropZeroes num
  | num == [] = ""
  | head num == '0' = dropZeroes (tail num)
  | otherwise = num

nbIncr :: NegaBinary -> NegaBinary
nbIncr x = (dropZeroes . reverse . nbIncrReal . reverse) ('0' : x)

nbIncrReal :: NegaBinary -> NegaBinary
nbIncrReal ('0' : t) = '1' : t
nbIncrReal ('1' : '1' : t) = "00" ++ t
nbIncrReal ('1' : '0' : t) = "01" ++ nbIncrReal t
nbIncrReal [] = "1"

mkCan :: NatP -> NatP
mkCan (NP (Z, a)) = (NP (Z, a))
mkCan (NP (a, Z)) = (NP (a, Z))
mkCan (NP ((S a), (S b))) = mkCan (NP (a, b))

{- </COPY> -}

{- Helper functions to convert the data types -}

intToNb :: Int -> NegaBinary
intToNb 0 = "0"
intToNb n = ((\(_,s)->s).intToNbReal) (n, "0")

intToNbReal :: (Int, NegaBinary) -> (Int, NegaBinary)
intToNbReal (0, n) = (0, n)
intToNbReal (i, n) = intToNbReal (i-1, nbIncr n)

natToNb :: Nat -> NegaBinary
natToNb a = (intToNb . (\(_,s)->s) . natToInt) (a, 0)

intToNat :: Int -> Nat
intToNat a = (\(_,s)->s) (intToNatReal (a, Z))

intToNatReal :: (Int, Nat) -> (Int, Nat)
intToNatReal (0, s) = (0, s)
intToNatReal (a, b) = intToNatReal (a-1, (S b))

{- First point of this part: Write a show function for RatNumbers -}

instance Show RatNumbers where
	show (Rat a b) = (natToNb a) ++ "/" ++ (natToNb b)

{- Second point: Make RatNumbers and NatP instances of Nf ('Normalform') and implement the function -}

instance Nf RatNumbers where
	t2nf (Rat Z _) = (Rat Z (S Z))
	t2nf (Rat ao bo) = (Rat (intToNat as) (intToNat bs))
		where
			a = ((\(_,s)->s) . natToInt) (ao, 0)
			b = ((\(_,s)->s) . natToInt) (bo, 0)
			as = div a (euclid a b)
			bs = div b (euclid a b)

instance Nf NatP where
	t2nf (NP (a, b)) = mkCan (NP (a, b))
