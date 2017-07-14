{-# LANGUAGE RankNTypes, TypeSynonymInstances #-}
module Rep where

type T a b c = forall f. Functor f => (a -> f b) -> f c

newtype R a b c = R (a, b -> c)

instance Functor (R a b) where
    fmap f (R (a, g)) = R (a, f . g)

phi' :: R a b c -> T a b c
phi' (R (a, k)) = \g -> fmap k (g a)

--------------------------------------------------------------

class Functor f => Pointed f where
    etaX :: c -> f c
