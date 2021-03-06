module Data.Git.Enumeratee.LinesPerDay where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Lens.Common (getL)
import Data.Enumerator (
    Enumeratee (..)
  , Iteratee (..)
  , Step (..)
  , Stream (..)
  , (>>==)
  , continue
  , yield
  )

import Data.Git.Types

class Monad m => HasLinesPerDay m where
  registerLPD :: CommitAuthor -> CommitDate -> Integer -> m ()


trackLinesPerDay :: HasLinesPerDay m 
                 => [ByteString] 
                 -> Enumeratee GitCommit GitCommit m b
trackLinesPerDay _ step@(Yield {}) = yield step EOF
trackLinesPerDay forbiddenPaths step@(Continue consumer) = continue go
  where
    go stream@(Chunks cs) = Iteratee $ do
      forM_ cs $ \commit -> 
        registerLPD (getL commitAuthor commit)
                    (getL commitDate   commit)
                    (getLimitedAddedLines forbiddenPaths commit)
      runIteratee $ consumer stream >>== trackLinesPerDay forbiddenPaths
    go EOF = consumer EOF >>== \step -> yield step EOF
