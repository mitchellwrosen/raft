{-# language UnicodeSyntax #-}

module IxFree where

data Free f i a where
  Pure :: a i -> Free f i a
  Free :: f i a -> (∀j. a j -> Free f j b) -> Free f i b

(>>=>)
  :: (∀i. a i -> Free f i b)
  -> (∀i. b i -> Free f i c)
  -> (∀i. a i -> Free f i c)
(f >>=> g) a = f a >>>= g

(>>>=) :: Free f i a -> (∀j. a j -> Free f j b) -> Free f i b
Pure a   >>>= f = f a
Free x k >>>= f = Free x (k >>=> f)

liftF :: f i a -> Free f i a
liftF x = Free x Pure

data (:=) a b c where
  V :: a -> (a := b) b

data Is a b where
  Is :: Is a a

data (:+) a b i where
  L :: a i -> (a :+ b) i
  R :: b i -> (a :+ b) i
infixr :+
