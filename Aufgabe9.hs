
type Country = String
type Countries = [Country]
type TravelTime = Integer -- Travel time in minutes
data Connection = Air Country Country TravelTime
	| Sea Country Country TravelTime
	| Rail Country Country TravelTime
	| Road Country Country TravelTime deriving (Eq,Ord,Show)
type Connections = [Connection]
data Itinerary = NoRoute | Route (Connections,TravelTime) deriving (Eq,Ord,Show)


-- SECOND PART

luckyNumbers :: [Integer]
luckyNumbers = luckyN

{-
 Start with 3 and keep a list of Boolean values if number is accepted or not
-}
luckyN = (loop 3 0 (repeat True))
	where
		loop n i (False:t) = loop (n+2) (i+2) t
		loop n i (_:t) = n:(loop (n+2) (i+1) (markFalse i (n-1) t))


-- markFalse mark every (n+1)-th number as false (skipping all already marked numbers)
markFalse :: Integer -> Integer -> [Bool] -> [Bool]
markFalse i n (False:t) = False: markFalse i n t
markFalse 0 n (_:t) = False: markFalse n n t
markFalse i n (_:t) = True:  markFalse (i-1) n t

-- THIRD PART

isLuckyNumber :: Integer -> Bool
isLuckyNumber n = n == (head (dropWhile (< n) luckyNumbers))

yieldLuckyNumbers :: Integer -> Integer -> [Integer]
yieldLuckyNumbers a b
	| b < a = []
	| a == b = [a | isLuckyNumber a]
	| otherwise = (yieldLuckyNumbers a a) ++ (yieldLuckyNumbers (a+1) b)

isTwinLuckyNumber :: Integer -> Bool
isTwinLuckyNumber n
	| isLuckyNumber n == False = False
	| or (map isLuckyNumber [n-2,n+2]) = True
	| otherwise = False
