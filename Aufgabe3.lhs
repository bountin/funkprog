> module Aufgabe3 where

Number examples:

0 = "0"
1 = "1"
2 = "110"
3 = "111"
4 = "100"
5 = "101"
6 = "11010"
...

-1 = "11"
-2 = "10"
-3 = "1101"
-4 = "1100"
-5 = "1111"
-6 = "1110"
-7 = "1001"
-8 = "1000"
-9 = "1011"
-10 = "1010"
-11 = "110101"
...

============================================

***************************************************************************************
**** NegaBinary: Type definition of a (-2) based Number. It is handled as a String ****
***************************************************************************************

> type NegaBinary = String

***************************
**** clearLeadingZeros ****
***************************

Drop leading zeros of a NegaBianry which have no semantic and so that there is only one reprensentation of a number in
our NegaBinary system.

> dropZeroes :: NegaBinary -> NegaBinary
> dropZeroes input = if (dropped == "") then "0" else dropped
>       where dropped = realDropZeroes input

> realDropZeroes num
>   | num == [] = ""
>   | head num == '0' = dropZeroes (tail num)
>   | otherwise = num

***************************************************
**** Extract a NegaBinary from a given string. ****
***************************************************

ExtractReal does the real extraction process and extract is a wrapper to return the value "0" if extractReal can't find
any NegaBinary.

> extractReal :: String -> NegaBinary
> extractReal "" = ""
> extractReal ('0':t) = '0' : extractReal t
> extractReal ('1':t) = '1' : extractReal t
> extractReal (_:t) = extractReal t

> extract :: String -> NegaBinary
> extract input = if (extracted == "") then "0" else extracted
>	where extracted = (dropZeroes.extractReal) input


****************************************************
**** nbIsPos: Check if a NegaBinary is positive ****
****************************************************

To find out if a NegaBinary is positive or negative, we can count the characters. Odd number means a positive number,
an even length a negative number (With the special case "0" wich is defined as positive).

> nbIsPos :: NegaBinary -> Bool
> nbIsPos number = 1 == mod (length (dropZeroes number)) 2

***************************************
**** nbIncr: Increase a Negabinary ****
***************************************

There are the following cases if you want to increment a NegaBinary:

  00        01      10      11
+ 01      + 01    + 01    + 01
  --        --      --      --
  01      1 10      11      00


> nbIncr :: NegaBinary -> NegaBinary
> nbIncr x = (dropZeroes . reverse . nbIncrReal . reverse) ('0' : x)

> nbIncrReal :: NegaBinary -> NegaBinary
> nbIncrReal ('0' : t) = '1' : t
> nbIncrReal ('1' : '1' : t) = "00" ++ t
> nbIncrReal ('1' : '0' : t) = "01" ++ nbIncrReal t
> nbIncrReal [] = "1"

***************************************
**** nbDecr: Decrease a Negabinary ****
***************************************

There are the following cases if you want to decrease a NegaBinary:

  00        01    1 10      11
- 01      - 01    - 01    - 01
  --        --      --      --
  11        00      01      10

> nbDecr :: NegaBinary -> NegaBinary
> nbDecr x = (dropZeroes . reverse . nbDecrReal . reverse) ('0' : x)

> nbDecrReal :: NegaBinary -> NegaBinary
> nbDecrReal ( '0' : '0' : t) = "11" ++ t
> nbDecrReal ( '1' : '0' : t) = "00" ++ t
> nbDecrReal ( '1' : '1' : t) = "01" ++ t
> nbDecrReal ( '0' : '1' : t) = "10" ++ nbDecrReal t
> nbDecrReal ( '0' : t ) = "11" ++ t
> nbDecrReal [] = "11"

***************************************************
**** nbAbs: Get absolute Value of a Negabinary ****
***************************************************

Positive numbers are not interesting, negative values are increased n times until 0 and then increaded n times again.

> nbAbs :: NegaBinary -> NegaBinary
> nbAbs num
>   | nbIsPos num = dropZeroes num
>   | num == "0" = num
>   | otherwise = nbAbsReal num

> nbAbsReal :: NegaBinary -> NegaBinary
> nbAbsReal num = ((\(a, b) -> a) . nbAbsCount) (num, 0)

> nbAbsCount :: (NegaBinary, Integer) -> (NegaBinary, Integer)
> nbAbsCount ("0", n) = nbAbsIncr ("0", n)
> nbAbsCount (num, n) = nbAbsCount (nbIncr num, n + 1)

> nbAbsIncr :: (NegaBinary, Integer) -> (NegaBinary, Integer)
> nbAbsIncr (num, 0) = (num, 0)
> nbAbsIncr (num, n) = nbAbsIncr (nbIncr num, n -1)

*************************************
**** nbPlus: Add to Negabinaries ****
*************************************

> nbPlus :: NegaBinary -> NegaBinary -> NegaBinary
> nbPlus "0" n = n
> nbPlus n "0" = n
> nbPlus a b
>	| nbIsPos a = nbPlus (nbDecr a) (nbIncr b)
>	| otherwise = nbPlus (nbIncr a) (nbDecr b)

****************************************
**** nbMinus: Subtract Negabinaries ****
****************************************

> nbMinus :: NegaBinary -> NegaBinary -> NegaBinary
> nbMinus n "0" = n
> nbMinus a b
>	| nbIsPos b = nbMinus (nbDecr a) (nbDecr b)
>	| otherwise = nbMinus (nbIncr a) (nbIncr b)

********************************************
**** nbTimes: Multiply two Negabinaries ****
********************************************

> nbTimes :: NegaBinary -> NegaBinary -> NegaBinary
> nbTimes "0" _ = "0"
> nbTimes _ "0" = "0"
> nbTimes "1" n = n
> nbTimes n "1" = n
> nbTimes a b = (\(x, y, sum) -> sum) (nbTimesReal (a, b, "0") )

> nbTimesReal :: (NegaBinary, NegaBinary, NegaBinary) -> (NegaBinary, NegaBinary, NegaBinary)
> nbTimesReal (a, b, sum)
>   | b == "0" = (a, b, sum)
>   | nbIsPos b = nbTimesReal (a, nbDecr b, nbPlus sum a)
>   | otherwise = nbTimesReal (a, nbIncr b, nbMinus sum a)
