module Aufgabe5 where

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
