{-# LANGUAGE TemplateHaskell #-}
module Data.Git.Types where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (isInfixOf)
import           Data.Lens.Common      (getL)
import           Data.Lens.Template    (makeLenses)
import           Data.Monoid           (Sum(..), mappend, mempty)
import           Data.Set              (Set)

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

getLimitedAddedLines :: [ByteString] -> GitCommit -> Integer
getLimitedAddedLines paths =
    getSum .
    Set.fold mappend mempty .
    Set.map (Sum . getL cAddedLines) .
    Set.filter (checkPathName paths) .
    getL commitFiles
  where
    checkPathName paths cfile =
        not $ any (getL cFileName cfile `isInfixOf`) paths

getAddedLines :: GitCommit -> Integer
getAddedLines =
    getSum .
    Set.fold mappend mempty .
    Set.map (Sum . getL cAddedLines) .
    getL commitFiles

