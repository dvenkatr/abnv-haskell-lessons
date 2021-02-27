module Alternative where

  import Control.Applicative

  data Maybe' a = Just' a | Nothing'

  instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)

  instance Applicative Maybe' where
    pure a                    = Just' a
    (<*>) (Just' f) (Just' a) = Just' (f a)
    (<*>) _ _                 = Nothing'

  instance Alternative Maybe' where
    empty                   = Nothing'
    (<|>) Nothing' Nothing' = Nothing'
    (<|>) (Just' m1) _      = Just' m1
    (<|>) _ (Just' m2)      = Just' m2



    
