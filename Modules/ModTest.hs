{-# LANGUAGE KindSignatures, TypeOperators, MultiParamTypeClasses, ConstraintKinds, FlexibleContexts #-}
module ModTest where
import Control.Applicative
import Data.Functor

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  {-# INLINE first #-}

  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  {-# INLINE second #-}

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  {-# INLINE bimap #-}

newtype Flip p a b = Flip { runFlip :: p b a }
  deriving (Eq,Ord,Show,Read)

instance Bifunctor p => Bifunctor (Flip p) where
  first f = Flip . second f . runFlip
  {-# INLINE first #-}
  second f = Flip . first f . runFlip
  {-# INLINE second #-}
  bimap f g = Flip . bimap g f . runFlip
  {-# INLINE bimap #-}

instance Bifunctor ((,,) x) where
  bimap f g ~(x, a, b) = (x, f a, g b)
  {-# INLINE bimap #-}

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap f g h = g . h . f

class (Profunctor p, Bifunctor ten) => TamModule ten p where
  leftAction  :: p a b -> p (c `ten` a) (c `ten` b)
  rightAction :: p a b -> p (a `ten` c) (b `ten` c)
  
type TamStrong p = TamModule (,) p
type TamChoice p = TamModule Either p

instance TamModule (,) (->) where
  leftAction f x = second f x 
  rightAction f x = first f x
  
instance TamModule Const (->) where
  leftAction f (Const a) = Const a 
  rightAction f (Const a) = Const (f a)

instance (Bifunctor p) => TamModule (Flip p) (->) where
  leftAction f x = second f x  
  rightAction f x = first f x

instance TamModule ((,,) a) (->) where
  leftAction f x = second f x 
  rightAction f x = first f x
