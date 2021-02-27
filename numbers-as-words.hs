module Number where

numListAsWord :: [Integer] -> String
numListAsWord nl = case nl of
    [-1] -> "" -- see checkException'
    [0] -> ""
    [1] -> "one"
    [2] -> "two"
    [3] -> "three"
    [4] -> "four"
    [5] -> "five"
    [6] -> "six"
    [7] -> "seven"
    [8] -> "eight"
    [9] -> "nine"
    [1,0] -> "ten"
    [1,1] -> "eleven"
    [1,2] -> "twelve"
    [1,3] -> "thirteen"
    [1,4] -> "fourteen"
    [1,5] -> "fifteen"
    [1,6] -> "sixteen"
    [1,7] -> "seventeen"
    [1,8] -> "eighteen"
    [1,9] -> "nineteen"
    [2,0] -> "twenty"
    [3,0] -> "thirty"
    [4,0] -> "forty"
    [5,0] -> "fifty"
    [6,0] -> "sixty"
    [7,0] -> "seventy"
    [8,0] -> "eighty"
    [9,0] -> "ninety"
    [1,0,0] -> "hundred"
    [1,0,0,0] -> "thousand"
    [1,0,0,0,0] -> "ten thousand"
    [1,0,0,0,0,0] -> "hundred thousand"
    [1,0,0,0,0,0,0] -> "million"
    [1,0,0,0,0,0,0,0] -> "ten million"
    [1,0,0,0,0,0,0,0,0] -> "hundred million"
    [1,0,0,0,0,0,0,0,0,0] -> "billion"
    _ -> "normal"

checkExceptions :: [Integer] -> ([Integer], [Integer])
checkExceptions numAsList
  | len == 3 && head numAsList /=0 = (take 1 numAsList, 1: drop 1 numAsList) -- hundreds
  | len == 4 || len == 7 || len == 10 = (take 1 numAsList, 1: drop 1 numAsList) -- thousands || millions || billions
  | len == 5 || len == 8 || len == 11 = (take 2 numAsList, 1: drop 2 numAsList) -- 10 thousands || 10 millions || 10 billions
  | len == 6 || len == 9 || len == 12 = (take 3 numAsList, 1: drop 3 numAsList) -- 100 thousands || 100 millions || 100 billions
  | len > 12 = (take (len - 9) numAsList, 1: drop (len - 9) numAsList)
  | otherwise = ([-1], numAsList)  -- rest
  where len = length numAsList

convertNumListtoWord :: [Integer] -> String
convertNumListtoWord [] = ""
convertNumListtoWord [-1] = ""
convertNumListtoWord numAsList
  | numAsList == [0] = "zero"
  | numListAsWord numAsList /= "normal" = numListAsWord numAsList
  | len == 1 = numListAsWord (take 1 numAsList) -- units
  | len == 2 && h == 0 = numListAsWord (tail numAsList)
  | len == 2 && h == 1 = numListAsWord numAsList -- 11 to 19
  | len == 2 && h /= 1 = numListAsWord [h, 0]
                     ++ " "
                     ++ numListAsWord (tail numAsList) -- 20 to 99
  | len == 3 && h == 0 = convertNumListtoWord (tail numAsList) -- 020 to 099
  | len == 3 && h == 1 = numListAsWord [1,0,0] ++ " " ++ writeNumListAsWord (tail numAsList) -- 1xx
  | len == 4 && h == 1 = numListAsWord [1,0,0,0] ++ " " ++ writeNumListAsWord (tail numAsList) -- 1,xxx
  | len == 7 && h == 1 = numListAsWord [1,0,0,0,0,0,0] ++ " " ++ writeNumListAsWord (tail numAsList) -- 1,xxx,xxx
  | len == 10 && h == 1 = numListAsWord [1,0,0,0,0,0,0,0,0,0] ++ " " ++ writeNumListAsWord (tail numAsList) -- 1,xxx,xxx,xxx
  | otherwise = writeNumListAsWord numAsList
  where
    len = length numAsList
    h = head numAsList

writeNumListAsWord :: [Integer] -> String
writeNumListAsWord numAsList = convertNumListtoWord (fst (checkExceptions numAsList))
                               ++ " "
                               ++ convertNumListtoWord (snd (checkExceptions numAsList))


writeNumAsWord :: Integer -> String
writeNumAsWord n = writeNumListAsWord (convertNumtoList n [])
 where
   convertNumtoList n numAsList =
    if div n 10 == 0
    then rem n 10 : numAsList
    else convertNumtoList (div n 10) (rem n 10 : numAsList)
