{-# LANGUAGE TemplateHaskell #-}
module Data.Git.Types where

import           Data.ByteString    (ByteString)
import           Data.Lens.Common   (getL)
import           Data.Lens.Template (makeLenses)
import           Data.Monoid        (Sum(..), mappend, mempty)
import           Data.Set           (Set)

import qualified Data.Set as Set

type CommitHash = ByteString
type CommitAuthor = ByteString
type CommitDate = ByteString

data CommitFile
  = CommitFile {
    _cAddedLines   :: Integer
  , _cRemovedLines :: Integer
  , _cFileName     :: ByteString
  }
  deriving (Show, Eq, Ord)

data GitCommit
  = GitCommit  {
    _commitRepo   :: String
  , _commitHash   :: CommitHash
  , _commitAuthor :: CommitAuthor
  , _commitDate   :: CommitDate
  , _commitFiles  :: Set CommitFile
  }
  deriving (Show)

makeLenses [''GitCommit, ''CommitFile]

getAddedLines :: GitCommit -> Integer
getAddedLines =
    getSum .
    Set.fold mappend mempty .
    Set.map (Sum . getL cAddedLines) .
    getL commitFiles

