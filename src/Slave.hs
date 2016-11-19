{-# options_ghc -fno-warn-unticked-promoted-constructors #-}

module Slave where

import IxFree
import Types

import Data.Kind (Type)

-- | (Kind) A 'Cell' has room for one existentially quantified thing.
data Cell where
  Empty :: Cell
  Full  :: a -> Cell -- a :: Ty

-- | The result of an 'Await': nothing, or a typed 'Request'.
data AwaitR :: Cell -> Type where
  TimedOut   :: AwaitR Empty
  GotRequest :: Request FromServer ty -> AwaitR (Full ty)

-- | A 'Slave' is a node in the follower or candidate state, wherein each
-- request received must be responded to immediately.
data SlaveF :: (Type -> Type) -> Cell -> (Cell -> Type) -> Type where
  -- Await only while there are no outstanding requests.
  Await :: SlaveF f Empty AwaitR

  -- Yield a response of the right type to the outstanding request, which
  -- empties the cell.
  Yield :: Response ty -> SlaveF f (Full ty) (Is Empty)

  -- Some other non-state-changing "ordinary" action. This is necessary because
  -- I haven't yet figured out how to properly compose things like SlaveF.
  Other :: f a -> SlaveF f s (a := s)

await :: Free (SlaveF f) Empty AwaitR
await = liftF Await

yield :: Response ty -> Free (SlaveF f) (Full ty) (Is Empty)
yield resp = liftF (Yield resp)

other :: f a -> Free (SlaveF f) s (a := s)
other x = liftF (Other x)
