module Reverse where

--revers xs = if xs == []
--  then []
--  else let h = head xs
--           t = tail xs
--     in revers t ++ [h]

getFirstSpace anyPhrase =
  if anyPhrase == ""
  then -1
  else
    if head anyPhrase == ' '
    then 1
    else
      let anyPhrase' = tail anyPhrase
      in if getFirstSpace anyPhrase' == -1
         then -1
         else getFirstSpace anyPhrase' + 1

revers xs =
  if getFirstSpace xs == -1
  then xs
  else let index = getFirstSpace xs
           newString = take (index-1) xs -- "arun"
           firstWord = ' ' : newString -- " arun"
           rest = drop index xs -- "is a good boy"
       in revers rest ++ firstWord  -- "boy good a is arun"
