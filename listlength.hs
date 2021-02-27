module ListLength where

data List = Null | Cons Int List deriving Show

length' :: List -> Int
length' (Null) = 0
length' (Cons _ rest) = 1 + length' rest

sum' :: List -> Int
sum'(Null) = 0
sum' (Cons first rest) = first + sum' rest

prepend :: Int -> List -> List
prepend x Null = Cons x Null
prepend x list = Cons x list

deletefst :: List -> List
deletefst Null = Null
deletefst (Cons first rest) = rest

delete :: Int -> List -> List
delete x Null = Null
delete x (Cons first rest) =
  if x == first then (delete x rest)
  else Cons first (delete x rest)

find :: Int -> List -> Bool
find x Null = False
find x (Cons first rest) =
  if x == first then True
    else find x rest

-- list of anything
data List' x = Null' | Cons' x (List' x) deriving Show
