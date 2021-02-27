module TypeClass where

class Show' a where
  show' :: a -> String

data Color = Red | Green | Blue

instance Show' Color where
  show' Red = "R"
  show' Green = "G"
  show' Blue = "B"

data Maybe' a = Just' a | Nothing'

instance (Show' a) => Show' (Maybe' a) where
  show' Nothing' = "Nothing'"
  show' (Just' a) = "Just [" ++ show' a ++ "]"

instance Show' Bool where
  show' True = "T"
  show' False = "F"

data List a = Empty | Cons a (List a)

instance (Show' a) => Show' (List a) where -- constrained polymorphism
  show' l = "[" ++ show'' l ++ "]"
    where
      show'' Empty = ""
      show'' (Cons x Empty) = show' x
      show'' (Cons x xs) = show' x ++ ", " ++ show'' xs

instance Show' Int where
  show' a = show a

-- alternate implementation using show
-- instance (Show a) => Show' (List a) where
--   show' l = "[" ++ show' l ++ "]"
--     where
--       show' Empty = ""
--       show' (Cons x Empty) = show x
--       show' (Cons x xs) = show x ++ "," ++ show' xs

class Eq' a where
  equal :: a -> a -> Bool
  equal x y = not (notEqual x y)

  notEqual :: a -> a -> Bool
  notEqual x y = not (equal x y)

  {-# MINIMAL equal | notEqual #-} -- pragma

instance Eq' Color where
  equal Red Red       = True
  equal Green Green   = True
  equal Blue Blue     = True
  equal _ _           = False

instance (Eq' a) => Eq' (List a) where
  equal Empty Empty             = True
  equal (Cons x xs) (Cons y ys) = equal x y && equal xs ys
  equal _ _ = False -- equivalent to equal Empty _ = False equal _ Empty = False

instance Eq' Int where
  equal a b = a==b


data Dir = N
        | E
        | S
        | W -- deriving (Show, Enum)

instance Enum Dir where
  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3

  toEnum 0 = N
  toEnum 1 = E
  toEnum 2 = S
  toEnum 3 = W
  toEnum n = toEnum (n `mod` 4)

instance Show Dir where
  show N = "North"
  show E = "East"
  show S = "South"
  show W = "West"
