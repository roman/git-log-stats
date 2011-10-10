module Data.Git.Enumerator where

-------------------------------------------------------------------------------

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Attoparsec.Lazy as P
import           Data.Enumerator (
    Iteratee (..)
  , Step (..)
  , Enumerator
  , Stream (..)
  , returnI
  )
import           System.FilePath (FilePath)

import qualified HSH as Shell

-------------------------------------------------------------------------------
import           System.Console.Options (CommandFlag(..))
import           Data.Git.Types
import qualified Data.Git.Parser as GP

enumGitLog :: MonadIO m
           => [CommandFlag]
           -> String
           -> Enumerator GitCommit m b
enumGitLog _ _ step@(Yield {})= returnI step
enumGitLog commandOpts0 repoPath (Continue consumer) = Iteratee $ do
    repoName <- getRepoName repoPath
    output   <- getCommitLogs
    let result = P.parse (GP.parse repoName) output
    case result of
      P.Done _ logs -> runIteratee $ consumer (Chunks logs)
      P.Fail _ _ e  -> error e
  where
    getRepoName = liftIO .
                  fmap Shell.basename .
                  Shell.abspath
    getCommitLogs = liftIO .
                    Shell.bracketCD repoPath .
                    Shell.run $
                    gitLogCommand commandOpts0

gitLogCommand :: [CommandFlag] -> String
gitLogCommand options0 = "git log " ++ options
  where
    options1 = ShortStat
             : LogFormat "%H, %an, %ai"
             : NoMerges
             : options0
    options = unwords $ map show options1

