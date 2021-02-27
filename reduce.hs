module Summary where

reduce :: (t -> a -> t) -> t -> [a] -> t -- fold
reduce _f a [] = a
reduce f a (x:xs) = reduce f (f a x) xs

sum' :: Int -> Int -> Int
sum' a x = a + x

prod :: Int -> Int -> Int
prod a x = a * x

len :: Int -> a -> Int
len a _x = a + 1
