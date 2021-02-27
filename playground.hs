module Example where

-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] -- 20

findPrimes n = findPrimeFactors allFactors
  where allFactors = [2 .. n]

findPrimeFactors (x:xs) =
  if dropMultiples (x:xs) x

dropMultiples [] _factor = []
dropMultiples (x:xs) factor =
  if rem x factor /= 0 then x : dropMultiples xs factor -- not a multiple
    else dropMultiples xs factor -- is a multiple
