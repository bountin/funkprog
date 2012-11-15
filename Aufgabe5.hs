module Aufgabe5 where

{- Needed for nub in erdos part -}
import List

data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

{- tmap: map on trees - not that kind of maps ;) -}

tmap :: (Label -> Label) -> Tree -> Tree
tmap _ Null = Null
tmap f (Tree label tree1 tree2) = Tree (f label) (tmap f tree1) (tmap f tree2)

{- tzw: zipWith on trees -}

tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzw f (Tree l1 tl1 tr1) (Tree l2 tl2 tr2) = Tree (f l1 l2) (tzw f tl1 tl2) (tzw f tr1 tr2)
tzw f _ _ = Null

{- tfold: fold on trees -}

tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
tfold f label (Tree l t1 t2) = f l (tfold f label t1) (tfold f label t2)
tfold _ label Null = label

{- Variables for testing purpose
f1 = \x y z -> x+y+z
f2 = \x y z -> x*y*z
t1 = Null
t2 = Tree 2 (Tree 3 Null Null) (Tree 5 Null Null)
t3 = Tree 2 (Tree 3 (Tree 5 Null Null) Null) (Tree 7 Null Null)
-}

{- ************** SECOND PART WITH ERDOS ******************** -}
{- TODO: Write a paper with a person who has written a paper with Erdos -}

type ErdosNumber = Integer
data Scientist = Sc Initial SurName  deriving (Eq,Show)
type Initial = Char
type SurName = String
type Author = Scientist
newtype Database = Db [([Author],PaperTitle)]
type PaperTitle = String

erdosNum :: Database -> Scientist -> Integer
erdosNum db sc = (\(number, _) -> number) (erdosSearch db (0, [sc]))

erdosSearch :: Database -> (Integer, [Scientist]) -> (Integer, [Scientist])
erdosSearch db (n, scientists)
	| scientists == [] = (-1, [])
	| or (map isErdos scientists) = (n, scientists)
	| otherwise = erdosSearch db (n+1, extendScientists db scientists)

isErdos :: Scientist -> Bool
isErdos (Sc 'P' "Erdos") = True
isErdos (Sc _ _) = False

extendScientists :: Database -> [Scientist] -> [Scientist]
extendScientists db orig
	| length (nub orig) == length new_scientists = []
	| otherwise = new_scientists
	where new_scientists = nub [sc | x <- orig, sc <- getCoworkers db x]

getCoworkers :: Database -> Scientist -> [Scientist]
getCoworkers db sc = [ x |
	papers <-  (\(Db d)->d) db,
	x <- (if sc `elem` (\(a,b)->a) papers then (\(a,b)->a) papers else [])  ]


{- Testing papers
db = Db [([Sc 'M' "Smith",Sc 'G' "Martin",Sc 'P' "Erdos"],"Newtonian Forms of Prime Factors"),
         ([Sc 'P' "Erdos",Sc 'W' "Reisig"],"Stuttering in Petri Nets") ,
         ([Sc 'M' "Smith",Sc 'X' "Chen"],"First Order Derivates in Structured Programming"),
         ([Sc 'T' "Jablonski",Sc 'Z' "Hsueh"],"Selfstabilizing Data Structures"),
         ([Sc 'X' "Chen",Sc 'L' "Li"],"Prime Numbers and Beyond")]
-}
