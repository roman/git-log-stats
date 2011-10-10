{-# LANGUAGE TemplateHaskell #-}
module Data.Git.Types where

import Data.ByteString (ByteString)
import Data.Lens.Template (makeLenses)

type CommitHash = ByteString
type CommitAuthor = ByteString
type CommitDate = ByteString

data GitCommit 
  = GitCommit  {
    _commitRepo   :: String
  , _commitHash   :: CommitHash
  , _commitAuthor :: CommitAuthor
  , _commitDate   :: CommitDate
  , _addedLines   :: Integer
  , _removedLines :: Integer
  }
  deriving (Show)

makeLenses [''GitCommit]
