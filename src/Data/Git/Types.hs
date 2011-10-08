{-# LANGUAGE TemplateHaskell #-}
module Data.Git.Types where

import Data.ByteString (ByteString)
import Data.Lens.Template (makeLenses)

data GitLog 
  = GitLog  {
    _commitHash   :: ByteString
  , _commitAuthor :: ByteString
  , _commitDate   :: ByteString
  , _addedLines   :: Int
  , _removedLines :: Int
  }
  deriving (Show)

makeLenses [''GitLog]
