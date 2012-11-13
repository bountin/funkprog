funkprog
========

``fp_lu01_121017.pdf`` contains description of the first assignment which is coded in ``Aufgabe1.hs``.


Dependencies
============

First, you need to install QuickCheck from cabal.

If you don't have cabal, get it from your repositories:

```sudo aptitude install cabal-install``` OR ```sudo ports install hs-cabal```

Then, update your cache and install QuickCheck:

```
cabal update
cabal install QuickCheck
```

Usage
=====

Now, make sure that your code exports all functions (put `module Aufgabe4 where` at the top), then run the test script you want:

```
runhaskell Aufgabe4_test.hs
```

(in this case Aufgabe4.lhs needs to somewhere on the search path, easiest way is to put it into the same directory)


Contributions
=============

Code is by my one (Bountin), tests are shamelessly stolen from Mononofu's repository [https://github.com/Mononofu/funprog]
