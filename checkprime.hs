-- A function to take an integer and return a bool representing whether it is a prime number or not

module Primes where

checkPrime x = checkPrime' x (takeWhile (\n -> n*n <= x) [2 ..])

checkPrime' x factors =
  if factors == []
  then True
  else if rem x (head factors) == 0
       then False
       else checkPrime' x (tail factors)


-- generate n prime numbers
generatePrimes n = take n [i | i <- [2..], checkPrime i]


-- generate all primes up to n
-- primesUpton = takeWhile (\x -> x <= n) [i | i <- [2..], checkPrime i]
primesUpto n = eratosthenes [2 .. n]

eratosthenes [] = []
eratosthenes (x:xs) = x : eratosthenes (dropMultiples x xs)

--sieve [] = []
--sieve (x:xs) = x : seive (filter (\a -> a `mod` x /= 0) xs)

dropMultiples _factor [] = []
dropMultiples factor (l:ls) =
  if l == factor || rem l factor == 0 then dropMultiples factor ls
    else l : dropMultiples factor ls


-- find prime factors of a number
primeFactorsOf :: Int -> [Int]
primeFactorsOf n = dropNonPrimeFactors n (primesUpto (n `div` 2))

dropNonPrimeFactors :: Int -> [Int] -> [Int]
dropNonPrimeFactors _n [] = []
dropNonPrimeFactors n list =
  if rem n (head list) /= 0 then dropNonPrimeFactors n (tail list)
    else head list : dropNonPrimeFactors n (tail list)
