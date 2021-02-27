-- Write a fibonacci function to generate a list of n numbers

module Fib where

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fiblist 0 = [1]
fiblist n = fib n : fiblist (n-1)

-- in command line
let fib n = if n<=1 then 1 else fib(n-1) + fib(n-2)
