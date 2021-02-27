-- write a data structure for a list of integers
data List = IntList Int List | Null deriving Show
data List = (:+:) Int List | Null deriving Show -- define your own symbolic function :+:
data List = Int :+: List | Null deriving Show -- define it as an infix operator
infixl 9 (:+:)

-- generic List
data IntL = IntList Int IntL | IntNull
data CharL = CharList Char CharL | CharNull
data List = IntL | CharL

-- why use newtype
newtype Age = Age Int deriving Show
let newAge a = if a > 120 then error "too old" else Age a
:t age 100 -- works
:t age 150 -- throws exception

-- person
data Person = Person { name :: [Char], age :: Int, loc :: Point }

-- person
data Person = Person { age :: Int }
let p = Person 10
age p
10 -- ans

let (Person a) = p
a
10 -- ans
