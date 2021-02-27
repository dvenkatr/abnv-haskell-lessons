
module Mon where

newtype Add = Add Int deriving (Show)
instance Monoid Add where
  mempty                    = Add 0
  (Add x) `mappend` (Add y) = Add (x+y)

newtype Prod = Prod Int deriving (Show)
instance Monoid Prod where
  mempty                      = Prod 1
  (Prod x) `mappend` (Prod y) = Prod (x*y)

data Maybe' a = Just' a | Nothing' deriving (Show)
instance Monoid a => Monoid (Maybe' a) where
  mempty = Nothing'
  Nothing' `mappend` Nothing'   = Nothing'
  Nothing' `mappend` Just' x    = Just' x
  Just' x `mappend` Nothing'    = Just' x
  (Just' x) `mappend` (Just' y) = Just' (x `mappend` y)
  mconcat                       = foldr mappend mempty

-- Just' (Prod 10) <> Just' (Prod 5)
-- Just' (Prod 50)

newtype MaxInt = MaxInt Int
instance Monoid MaxInt where
  mempty                          = MaxInt (minBound :: Int)
  (MaxInt x) `mappend` (MaxInt y) = MaxInt (max x y)

newtype MinInt = MinInt Int deriving Show
instance Monoid MinInt where
  mempty                          = MinInt (maxBound :: Int)
  (MinInt x) `mappend` (MinInt y) = MinInt (min x y)

newtype Max' a = Max' a
instance (Ord a, Bounded a) => Monoid (Max' a) where
  mempty                  = Max' minBound
  Max' x `mappend` Max' y = Max' (max x y)

newtype Min' a = Min' a
instance (Ord a, Bounded a) => Monoid (Min' a) where
  mempty = Min' maxBound
  Min' x `mappend` Min' y = Min' (min x y)

newtype Any' = Any' Bool deriving Show
instance Monoid Any' where
  mempty = Any' False
  (Any' x) `mappend` (Any' y) = Any' (x||y)

newtype All' = All' Bool deriving Show
instance Monoid All' where
  mempty = All' True
  (All' x) `mappend` (All' y) = All' (x&&y)


newtype First'' a = First' (Maybe a) deriving Show

instance Monoid (First'' a) where
  mempty = First' Nothing
  (First' Nothing) `mappend` (First' Nothing)   = First' Nothing
  (First' (Just a)) `mappend` (First' Nothing)  = First' (Just a)
  --(First' Nothing) `mappend` (First' (Just a))  = First' (Just a)
  (First' (Just a)) `mappend` (First' _)        = First' (Just a)


newtype Last'' a = Last' (Maybe a) deriving Show

instance Monoid (Last'' a) where
  mempty = Last' Nothing
  (Last' Nothing) `mappend` (Last' Nothing)   = Last' Nothing
  (Last' (Just a)) `mappend` (Last' Nothing)  = Last' (Just a)
  (Last' Nothing) `mappend` (Last' (Just a))  = Last' (Just a)
  (Last' _) `mappend` (Last' (Just b))        = Last' (Just b)



data List a = Cons a (List a) | Empty

instance Monoid (List a) where
  mempty = Empty

  --Empty `mappend` Empty = Empty -- redundant
  a `mappend` Empty     = a
  --Empty `mappend` b   = b -- redundant
  Cons a as `mappend` b = Cons a (as `mappend` b) -- separate function


data BST a = BSTCons a (BST a) (BST a) | EmptyBST

instance Monoid (BST a) where
  mempty = EmptyBST

  --Empty `mappend` Empty = Empty -- redundant
  a `mappend` Empty     = a
  --Empty `mappend` b     = b -- redundant
  Cons a as `mappend` b = Cons a (as `mappend` b)






-- laws
-- mappend mempty x = x
-- mappend x mempty = x
-- mappend x (mappend y z) = (mappend x y) z
-- mconcat = foldr mappend mempty


-- x + 0 = x
-- 0 + x = x
-- x + (y + z) = (x + y) + z
-- mconcat = foldr + 0


-- other monoids: list, maybe
