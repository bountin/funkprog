module Main where
	import List

	-- kgv: Berechnung des kleinsten gemeinsamen Vielfachen (mittels eukl. Algorithmus)
	kgv :: Integer -> Integer -> Integer
	kgv 0 _ = 0
	kgv _ 0 = 0
	kgv a b
		| a > 0     = div (a*b) (euclid a b)
		| otherwise = kgv (-a) b

	euclid :: Integer -> Integer -> Integer
	euclid a 0 = a
	euclid a b = euclid b (mod a b)

	agv :: Integer -> Integer -> (Integer, Integer) -> [Integer]
	agv 0 _ _       = []
	agv _ 0 _       = []
	-- dropWhile and takeWhile reduces the list to the given interval
	agv m n (p, v)  = dropWhile (< p) (takeWhile (<= v) (map (* (kgv m n)) [1..]))

	type PassName = String
	type FlightNumber = Integer
	type PlaceOfDeparture = String
	type Destination = String
	type Airfare = Integer
	type Flight = (PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)
	type Database = [Flight]

	name :: Flight -> PassName
	name (a,b,c,d,e) = a
	number :: Flight -> FlightNumber
	number (a,b,c,d,e) = b
	departure :: Flight -> PlaceOfDeparture
	departure (a,b,c,d,e) = c
	destination :: Flight -> Destination
	destination (a,b,c,d,e) = d
	fare :: Flight -> Airfare
	fare (a,b,c,d,e) = e


	flights :: Database -> PassName -> [(FlightNumber, Airfare)]
	flights db search_name =
		sortBy
			(\(n1, f1) (n2, f2) -> compare n1 n2) -- The sort function
			(map (\x -> (number x, fare x)) [f | f <- db, search_name == name f]) -- The data

	pass2Dest:: Database -> Destination -> [PassName]
	pass2Dest db search_dest = sort (map (name) [f | f <- db, search_dest == destination f])

	mostValuedPass :: Database -> PlaceOfDeparture -> Destination -> ([PassName], Airfare)
	mostValuedPass db search_dep search_dest = (reverse (sort (map (name) filtered_flights)), maxFare)
		where
		sorted_flights   = findByDepDest db search_dep search_dest
		filtered_flights = [f | f <- sorted_flights, maxFare == fare f]
		maxFare  = fare (head sorted_flights)

	findByDepDest :: Database -> PlaceOfDeparture -> Destination -> [Flight]
	findByDepDest db search_dep search_dest = sortBy (\a b -> compare (fare b) (fare a)) [f | f <- db, search_dest == destination f, search_dep == departure f]
