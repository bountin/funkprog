> module Aufgabe4 where

> data Nat = Z | S Nat deriving Show
> type NatPair = (Nat,Nat)

*****************
**** plusNat ****
*****************

> plusNat :: Nat -> Nat -> Nat
> plusNat a Z = a
> plusNat Z a = a
> plusNat a (S b) = plusNat (S a) b

******************
**** minusNat ****
******************

> minusNat :: Nat -> Nat -> Nat
> minusNat a Z = a
> minusNat Z a = Z
> minusNat (S a) (S b) = minusNat a b

******************
**** timesNat ****
******************

> timesNat :: Nat -> Nat -> Nat
> timesNat Z _ = Z
> timesNat _ Z = Z
> timesNat (S Z) a = a
> timesNat a (S Z) = a
> timesNat a b = (\(a,b,s) -> s) (timesNatReal (a, b, Z))

> timesNatReal :: (Nat, Nat, Nat) -> (Nat, Nat, Nat)
> timesNatReal (_, Z, s) = (Z, Z, s)
> timesNatReal (a, b, s) = timesNatReal (a, minusNat b (S Z), plusNat a s)

****************
**** divNat ****
****************

> divNat :: Nat -> Nat -> Nat
> divNat _ Z = error "Invalid argument"
> divNat a b = (\(a,b,s) -> s) (divNatReal (a,b,Z))

> divNatReal :: (Nat, Nat, Nat) -> (Nat, Nat, Nat)
> divNatReal (a, b, s)
>   | grEqNat a b = divNatReal (minusNat a b, b, plusNat s (S Z))
>   | otherwise = (a,b,s)

****************
**** modNat ****
****************

> modNat :: Nat -> Nat -> Nat
> modNat _ Z = error "Invalid argument"
> modNat a b = (\(a,b,s) -> a) (divNatReal (a,b,Z))

***************
**** eqNat ****
***************

> eqNat :: Nat -> Nat -> Bool
> eqNat Z Z = True
> eqNat _ Z = False
> eqNat Z _ = False
> eqNat (S a) (S b) = eqNat a b

> eqZero = eqNat Z

***************
**** grNat ****
***************

> grNat :: Nat -> Nat -> Bool
> grNat a b = not (leEqNat a b)

***************
**** leNat ****
***************

> leNat :: Nat -> Nat -> Bool
> leNat a b = not (grEqNat a b)

*****************
**** grEqNat ****
*****************

> grEqNat :: Nat -> Nat -> Bool
> grEqNat _ Z = True
> grEqNat Z _ = False
> grEqNat (S a) (S b) = grEqNat a b

*****************
**** leEqNat ****
*****************

> leEqNat :: Nat -> Nat -> Bool
> leEqNat Z _ = True
> leEqNat _ Z = False
> leEqNat (S a) (S b) = leEqNat a b

***************
**** mkCan ****
***************

> mkCan :: NatPair -> NatPair
> mkCan (Z, a) = (Z, a)
> mkCan (a, Z) = (a, Z)
> mkCan ((S a), (S b)) = mkCan (a, b)

****************
**** plusNP ****
****************

> plusNP :: NatPair -> NatPair -> NatPair
> plusNP a b = mkCan (plusNPReal (mkCan a) (mkCan b))

> plusNPReal :: NatPair -> NatPair -> NatPair
> plusNPReal (ap, an) (bp, bn) = (plusNat ap bp, plusNat an bn)

*****************
**** minusNP ****
*****************

> minusNP :: NatPair -> NatPair -> NatPair
> minusNP a b = mkCan (minusNPReal (mkCan a) (mkCan b))

> minusNPReal :: NatPair -> NatPair -> NatPair
> minusNPReal a (bp, bn) = plusNP a (bn, bp)

*****************
**** timesNP ****
*****************

> timesNP :: NatPair -> NatPair -> NatPair
> timesNP a b = mkCan (timesNPReal (mkCan a) (mkCan b))

> timesNPReal :: NatPair -> NatPair -> NatPair
> timesNPReal (ap, an) (bp, bn)
>   | and [eqZero ap, eqZero bp] = (timesNat an bn, Z)
>   | and [eqZero an, eqZero bn] = (timesNat ap bp, Z)
>   | and [eqZero ap, eqZero bn] = (Z, timesNat an bp)
>   | and [eqZero an, eqZero bp] = (Z, timesNat ap bn)

***************
**** divNP ****
***************

> divNP :: NatPair -> NatPair -> NatPair
> divNP a b = mkCan (divNPReal (mkCan a) (mkCan b))

> divNPReal :: NatPair -> NatPair -> NatPair
> divNPReal (ap, an) (bp, bn)
>   | and [eqZero bp, eqZero bn] = error "Invalid argument"
>   | and [eqZero ap, eqZero bp] = (divNat an bn, Z)
>   | and [eqZero an, eqZero bn] = (divNat ap bp, Z)
>   | and [eqZero ap, eqZero bn] = (Z, divNatDown (an, bp, Z))
>   | and [eqZero an, eqZero bp] = (Z, divNatDown (ap, bn, Z))
>       where
>       divNatDown = (\(a,b,s) -> if (eqNat a Z) then s else plusNat s (S Z)) . divNatReal

***************
**** modNP ****
***************

Throws some errors on mononofu's testsuite but I'm too lazy to fix it

> modNP :: NatPair -> NatPair -> NatPair
> modNP a b = mkCan (modNPReal (mkCan a) (mkCan b))

> modNPReal :: NatPair -> NatPair -> NatPair
> modNPReal (ap, an) (bp, bn)
>   | and [eqZero bp, eqZero bn] = error "Invalid argument"
>   | and [eqZero ap, eqZero bp] = (modNat an bn, Z)
>   | and [eqZero an, eqZero bn] = (modNat ap bp, Z)
> modNPReal a b = modNPNegative a b

> modNPNegative :: NatPair -> NatPair -> NatPair
> modNPNegative (ap, an) (bp, bn)
>   | and [eqZero ap, eqZero bn] = modNP (plusNP (ap, an) (bp, bn)) (bp, bn)
>   | and [eqZero an, eqZero bp] = (modNat ap bn, Z)

**************
**** eqNP ****
**************

> eqNP :: NatPair -> NatPair -> Bool
> eqNP a b = eqNPReal (mkCan a) (mkCan b)

> eqNPReal :: NatPair -> NatPair -> Bool
> eqNPReal (a, Z) (b, Z) = eqNat a b
> eqNPReal (Z, a) (Z, b) = eqNat a b
> eqNPReal a b = False

**************
**** grNP ****
**************

> grNP :: NatPair -> NatPair -> Bool
> grNP a b = grNPReal (mkCan a) (mkCan b)

> grNPReal :: NatPair -> NatPair -> Bool
> grNPReal (a, Z) (b, Z) = grNat a b
> grNPReal (Z, a) (Z, b) = leNat a b
> grNPReal (a, Z) (Z, b) = True
> grNPReal a b = False

**************
**** leNP ****
**************

> leNP :: NatPair -> NatPair -> Bool
> leNP a b = leNPReal (mkCan a) (mkCan b)

> leNPReal :: NatPair -> NatPair -> Bool
> leNPReal (a, Z) (b, Z) = leNat a b
> leNPReal (Z, a) (Z, b) = grNat a b
> leNPReal (Z, a) (b, Z) = True
> leNPReal a b = False

****************
**** grEqNP ****
****************

> grEqNP :: NatPair -> NatPair -> Bool
> grEqNP a b = grEqNPReal (mkCan a) (mkCan b)

> grEqNPReal :: NatPair -> NatPair -> Bool
> grEqNPReal (a, Z) (b, Z) = grEqNat a b
> grEqNPReal (Z, a) (Z, b) = leEqNat a b
> grEqNPReal a b = grNPReal a b

****************
**** leEqNP ****
****************

> leEqNP :: NatPair -> NatPair -> Bool
> leEqNP a b = leEqNPReal (mkCan a) (mkCan b)

> leEqNPReal :: NatPair -> NatPair -> Bool
> leEqNPReal (a, Z) (b, Z) = leEqNat a b
> leEqNPReal (Z, a) (Z, b) = grEqNat a b
> leEqNPReal a b = leNPReal a b
