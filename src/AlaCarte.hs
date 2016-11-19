module AlaCarte where

import Data.Functor.Sum

type (:+) = Sum
infixr :+

class (Functor f, Functor g) => f :< g where
  inj :: f a -> g a

instance Functor f => f :< f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :< Sum f g where
  inj = InL

instance (Functor f, Functor g, Functor h, f :< g) => f :< Sum h g where
  inj = InR . inj
