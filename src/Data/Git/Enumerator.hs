module Data.Git.Enumerator where

-------------------------------------------------------------------------------

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.Attoparsec as P
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
           -> Enumerator GitCommit m b
enumGitLog _ step@(Yield {})= returnI step
enumGitLog commandOpts0 (Continue consumer) = Iteratee $ do
    output <- liftIO . Shell.run $ gitLogCommand commandOpts0
    let result = P.parseOnly GP.parse output
    case result of
      Right logs -> runIteratee $ consumer (Chunks logs)
      Left e     -> error e

gitLogCommand :: [CommandFlag] -> String
gitLogCommand options0 = "git log " ++ options
  where
    options1 = ShortStat 
             : LogFormat "%H, %ae, %ai" 
             : NoMerges 
             : options0
    options = unwords $ map show options1
    
