{-# language TemplateHaskell                             #-}
{-# options_ghc -fno-warn-unticked-promoted-constructors #-}

module Types where

import Data.Kind (Type)
import Lens.Micro.TH

type CandidateId = NodeId
type Command     = String
type Index       = Int
type NodeId      = String
type Term        = Int

-- (Kind) Type of a message
data Ty
  = TyCommand
  | TyAppendEntries
  | TyRequestVote

--------------------------------------------------------------------------------
-- Requests

-- (Kind) Source of a received message
data Source
  = FromClient
  | FromServer

data Request :: Source -> Ty -> Type where
  ReqClient        :: Command              -> Request FromClient TyCommand
  ReqAppendEntries :: AppendEntriesRequest -> Request FromServer TyAppendEntries
  ReqRequestVote   :: RequestVoteRequest   -> Request FromServer TyRequestVote

data AppendEntriesRequest = AppendEntriesReq
  { -- Index of log entry immediately preceding new ones
    _prevLogIndex :: Index

    -- Term of prevLogIndex entry
  , _prevLogTerm  :: Term

    -- Log entry to store (Nothing for heartbeat)
  , _entry        :: Maybe Command

    -- Leader's commit index
  , _leaderCommit :: Index
  } deriving Show

data RequestVoteRequest

--------------------------------------------------------------------------------
-- Responses

data Response :: Ty -> Type where
  RespClient        :: ClientResponse        -> Response TyCommand
  RespAppendEntries :: AppendEntriesResponse -> Response TyAppendEntries
  RespRequestVote   :: RequestVoteResponse   -> Response TyRequestVote

data AppendEntriesResponse = AppendEntriesResp
  { -- Current term, for leader to update itself
    _term :: Term

    -- True if follower contained entry matching prevLogIndex and prevLogTerm
  , _success :: Bool
  }

data RequestVoteResponse

data ClientResponse

--------------------------------------------------------------------------------

makeFields ''AppendEntriesRequest
makeFields ''AppendEntriesResponse
