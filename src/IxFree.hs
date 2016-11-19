module IxFree where

data Free f i a where
  Pure :: a i -> Free f i a
  Free :: f i a -> (∀j. a j -> Free f j b) -> Free f i b

imap :: (∀j. a j -> b j) -> Free f i a -> Free f i b
imap f (Pure a)   = Pure (f a)
imap f (Free x k) = Free x (imap f . k)

ipure :: a i -> Free f i a
ipure = Pure

(?>=>)
  :: (∀i. a i -> Free f i b)
  -> (∀i. b i -> Free f i c)
  -> (∀i. a i -> Free f i c)
(f ?>=> g) a = f a ?>>= g

-- | The demonic bind.
(?>>=) :: Free f i a -> (∀j. a j -> Free f j b) -> Free f i b
Pure a   ?>>= f = f a
Free x k ?>>= f = Free x (k ?>=> f)

-- | The angelic bind.
(!>>=) :: Free f i (x := j) -> (x -> Free f j b) -> Free f i b
Pure (V a) !>>= f = f a
Free x k   !>>= f = Free x (\y -> k y !>>= f)

liftF :: f i a -> Free f i a
liftF x = Free x Pure

data (:=) a b c where
  V :: a -> (a := b) b

data Is a b where
  Refl :: Is a a
