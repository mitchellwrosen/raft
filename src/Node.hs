{-# language RebindableSyntax #-}
{-# language TemplateHaskell  #-}

module Node where

import AlaCarte
import IxFree
import Slave
import Types

import Data.Kind
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH
import Prelude hiding ((>>=), pure)

pure :: x -> Free f i (x := i)
pure = Pure . V

(>>=) :: Free f i a -> (∀j. a j -> Free f j b) -> Free f i b
(>>=) = (?>>=)


data NodeEnv = NodeEnv
  { _nodeId :: NodeId
  , _nodes :: [NodeId]
  }
makeLenses ''NodeEnv

data NodeState = NodeState
  { -- Last term server has seen
    _currentTerm :: Term

    -- CandidateId that received vote in current term (or Nothing if none)
  , _votedFor    :: Maybe CandidateId

    -- Log entries; each entry contains command for state machine, and term when
    -- entry was received by leader (first index is 1)
  , _entries     :: [(Command, Index)]

  , _leader      :: NodeId

    -- Index of highest log entry known to be committed (initialized to 0,
    -- increases monotonically
  , _commitIndex :: Index

    -- Index of highest log entry applied to state machine (initialized to 0,
    -- increases monotonically
  , _lastApplied :: Index
  }
makeLenses ''NodeState

data NodeF :: Type -> Type where
  AskNodeEnv      :: NodeF NodeEnv
  GetNodeState    :: NodeF NodeState
  ModifyNodeState :: (NodeState -> NodeState) -> NodeF NodeState

askNodeEnv :: NodeF :< f => Free (SlaveF f) s (NodeEnv := s)
askNodeEnv = other (inj AskNodeEnv)

modifyNodeState
  :: NodeF :< f
  => (NodeState -> NodeState) -> Free (SlaveF f) s (NodeState := s)
modifyNodeState f = other (inj (ModifyNodeState f))

--------------------------------------------------------------------------------
-- Derived API

askNodes :: NodeF :< f => Free (SlaveF f) s ([NodeId] := s)
askNodes =
  askNodeEnv !>>= \env ->
  pure (view nodes env)

askQuorum :: NodeF :< f => Free (SlaveF f) s (Int := s)
askQuorum =
  askNodes !>>= \ids ->
  pure (floor (fromIntegral (length ids) / (2 :: Double) + 1))

getCurrentTerm :: NodeF :< f => Free (SlaveF f) s (Term := s)
getCurrentTerm = modifyNodeState id !>>= \env -> pure (view currentTerm env)

putCurrentTerm :: NodeF :< f => Term -> Free (SlaveF f) s (Is s)
putCurrentTerm term =
  modifyNodeState (set currentTerm term) !>>= const (Pure Refl)

incrementCurrentTerm :: NodeF :< f => Free (SlaveF f) s (Is s)
incrementCurrentTerm =
  modifyNodeState (over currentTerm (+1)) !>>= const (Pure Refl)

getVotedFor :: NodeF :< f => Free (SlaveF f) s (Maybe NodeId := s)
getVotedFor = modifyNodeState id !>>= \env -> pure (view votedFor env)

putVotedFor :: NodeF :< f => Maybe NodeId -> Free (SlaveF f) s (Is s)
putVotedFor mid = modifyNodeState (set votedFor mid) !>>= const (Pure Refl)
