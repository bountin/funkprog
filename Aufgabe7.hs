module Aufgabe7 where
import Data.List

-- FIRST PART: Database

type Movie = (Title,Regisseur,MainActors,ReleaseDate,Genre,SalesPrice)
type Title = String
type Regisseur = String
type Actor = String
type MainActors = [Actor]
type ReleaseDate = Int
data Genre = Thriller | Fantasy | ScienceFiction | Comedy deriving (Eq,Ord,Show)
type SalesPrice = Int
type Database = [Movie]

{- Not workig but no time :-/
movie_eq :: Movie -> Movie -> Bool
movie_eq (t,r,m,d,g,s) (t2,r2,m2,d2,g2,s2) =

movie_elem :: Movie -> [Movie] -> Bool
movie_elem m [] = True
movie_elem m (x:xs)
	| m `movie_eq` x = False
	| otherwise = movie_elem m xs

rm_dup :: Database -> Database
rm_dup (x:xs)
	| x `elem` xs = rm_dup xs
	| otherwise = x : rm_dup xs
	-}

get_rtd :: Database -> [(Regisseur,Title,ReleaseDate)]
get_rtd db = map (\(t,r,_,d,_,_)->(r,t,d)) db

get_rtg :: Database -> [(Regisseur,Title,Genre)]
get_rtg db = map (\(t,r,_,_,g,_)->(r,t,g)) (filter (\(t,r,m,d,_,_) -> r `elem` m) db)

get_tad :: Database -> ReleaseDate -> [(Title,MainActors,ReleaseDate)]
get_tad db date =  map (\(t,r,m,d,g,s)->(t,m,d)) (filter (\(t,r,m,d,g,s)->d==date) db)

upd_dbri_incr :: Genre -> Regisseur -> Int -> Movie -> Movie
upd_dbri_incr genre regisseur delta_price (t,r,m,d,g,s)
	| and [genre == g, regisseur == r] = (t,r,m,d,g,max (s + delta_price) 1)
	| otherwise = (t,r,m,d,g,s)

upd_dbgri :: Database -> Genre -> Regisseur -> Int -> Database
upd_dbgri db genre regisseur delta_price = map (upd_dbri_incr genre regisseur delta_price) db

upd_dbad :: Database -> Actor -> ReleaseDate -> Database
upd_dbad db actor date = filter (\(t,r,m,d,g,s) -> and [d >= date, not (actor `elem` m)] ) db

get_dbda :: Database -> ReleaseDate -> Actor -> Database
get_dbda db date actor = filter (\(t,r,m,d,g,s) -> and [d <= date, not (actor `elem` m)]) db

compare_j :: Movie -> Movie -> Ordering
compare_j (_,_,_,d1,_,_) (_,_,_,d2,_,_) = compare d1 d2

sort_dbj :: Database -> Database
sort_dbj db = sortBy compare_j db

compare_genre_regie :: Movie -> Movie -> Ordering
compare_genre_regie (_,r1,_,_,g1,_) (_,r2,_,_,g2,_)
	| g1 == g2  = compare r1 r2
	| otherwise = compare g1 g2

sort_dbgr :: Database -> Database
sort_dbgr db = sortBy compare_genre_regie db

-- SECOND PART: Solve a game

{- Not working for more complex stuff -}

type ListOfValues = [Integer]
type TargetValue = Integer
type Game = (ListOfValues,TargetValue)
data Operators = Plus | Times | Minus | Div deriving (Eq,Ord,Show)
type Solution = [Operators]

solve :: Game -> Solution
solve ([],_) = []
solve (a, b) = addSolve (a, b) []

addSolve :: Game -> Solution -> Solution
addSolve ([a,b], z) sol
	| (a + b) == z = (Plus:sol)
	| otherwise = minusSolve ([a,b], z) sol
-- addSolve ((x:y:xs), z) sol = addSolve ((x+y):xs, z) (Plus:sol)

minusSolve :: Game -> Solution -> Solution
minusSolve ([a,b], z) sol
	| (a - b) == z = [Minus]
	| otherwise = timesSolve ([a,b], z) sol

timesSolve :: Game -> Solution -> Solution
timesSolve ([a,b], z) sol
	| (a * b) == z = [Times]
	| otherwise = divSolve ([a,b], z) sol

divSolve :: Game -> Solution -> Solution
divSolve ([a,b], z) _
	| (a `div` b) == z = [Div]
	| otherwise = []
