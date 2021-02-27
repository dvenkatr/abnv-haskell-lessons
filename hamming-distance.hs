module Distance where

hamming :: String -> String -> Int
hamming [] [] = 0
hamming [] _ = error "unequal"
hamming _ [] = error "unequal"
hamming (x:xs) (y:ys) =
    (if x /= y
      then 1
      else 0) + hamming xs ys

-- equivalent to
hamming' [] [] = 0
hamming' w1 w2 =
  if head w1 /= head w2
    then 1 + hamming (tail w1) (tail w2)
    else 0 + hamming (tail w1) (tail w2)

-- using tail recursion (last call is function itself)
recursiveHam [] [] acc = acc
recursiveHam (x:xs) (y:ys) acc =
  if x==y then recursiveHam xs ys acc
    else recursiveHam xs ys (acc+1)
