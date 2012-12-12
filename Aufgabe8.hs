module Aufgabe8 where
import Data.Char
import Hugs.Observe

-- First part

type InitialValue = Integer
type NumberOfRounds = Integer
type MaxRounds = Integer
type FinalValue = Integer
type TextRep = String
data Solution = Failure | Success (NumberOfRounds,FinalValue,TextRep) deriving (Eq, Show)

addRev :: InitialValue -> MaxRounds -> Solution
addRev i m = addRevReal (abs i) (abs m) 0

addRevReal :: InitialValue -> MaxRounds -> Integer -> Solution
addRevReal i m c
	| isPalindrome' (show i) = Success (c, i, show i)
	| m == 0 = Failure
	| otherwise = addRevReal (i+next) (m-1) (c+1)
	where
		next = (read.reverse.show) i :: Integer

isPalindrome :: String -> Bool
isPalindrome s
	| length s <= 1 = True
	| (head.reverse) s == head s = isPalindrome next
	| otherwise = False
	where
		i = read s :: Int
		next = (reverse.tail.reverse.tail) s

-- isPalindrome' = observe "X: " isPalindrome
isPalindrome' = isPalindrome
