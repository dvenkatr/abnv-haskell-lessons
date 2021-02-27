module Functor where

data Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


data Maybe' a = Just' a | Nothing'
instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)
-- (a->b) -> Maybe a -> Maybe b

instance Applicative Maybe' where
  pure a                    = Just' a
  (<*>) (Just' f) (Just' a) = Just' (f a)
  (<*>) _ _                 = Nothing'


data Either' a b = Left' a | Right' b
instance Functor (Either' a) where
  fmap _ (Left' a)  = Left' a
  fmap f (Right' a) = Right' (f a)
-- (x -> y) -> Either a x -> Either a y

instance Applicative (Either' a) where
  pure b                      = Right' b
  (<*>) (Right' f) (Right' a) = Right' (f a)
  (<*>) _ (Left' b)           = Left' b
  (<*>) (Left' f) _           = Left' f


data Option a = Option1 a | Option2 a
instance Functor Option where
  fmap f (Option1 x)  = Option1 (f x)
  fmap f (Option2 y)  = Option2 (f y)


data List a = Cons a (List a) | EmptyList
instance Functor List where
  fmap _ EmptyList               = EmptyList
  fmap f (Cons a as)             = Cons (f a) (fmap f as)

instance Monoid (List a) where
  mempty = EmptyList
  a `mappend` EmptyList  = a
  Cons a as `mappend` b  = Cons a (as `mappend` b) -- separate function

instance Applicative List where
  pure a                        = Cons a EmptyList --[a]
  (<*>) (Cons f fs) a           = fmap f a `mappend` (fs <*> a) -- mappend needs Monoid
  (<*>) _ _                     = EmptyList
  -- f(a->b) -> fa -> fb


data BST a = Node a (BST a) (BST a)| EmptyBST
instance Functor BST where
  fmap _ EmptyBST               = EmptyBST
  fmap f (Node a l r)           = Node (f a) (fmap f l) (fmap f r)


data Pair a b = Pair a b deriving Show
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
  -- fmap :: (a -> b) -> t a -> t b

-- bifunctor
--fmap' :: (a->c) -> (b->d) -> t a b -> t c d

f n a = n `mod` a == 0
checkIsPrime n = map (f n) [1, 2, 3]
