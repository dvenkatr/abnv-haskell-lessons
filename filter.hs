module Filter where

filter' :: (t->Bool)->[t]->[t]
filter' _f [] = []
filter' f (x:xs) = if f x
                    then x : filter' f xs
                    else filter' f xs

even' :: Int -> Bool
even' x = rem x 2 == 0

odd' :: Int -> Bool
odd' x = rem x 2 /= 0

--filter' (\x -> x `mod` 2 == 0) [1,2,3] -- anonymous function
--filter' (* 2) [1,2,3] -- [2,4,6]
--filter' (2 *) [1,2,3] -- [2,4,6]
