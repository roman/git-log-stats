module Data.Git.Enumerator where

-------------------------------------------------------------------------------

import           Data.Enumerator (Iteratee(..), Step(..), Enumerator, returnI)
import           System.FilePath (FilePath)

-- import qualified HSH as Shell

-------------------------------------------------------------------------------
import           System.Console.Options (CommandFlag(..))
import           Data.Git.Types 
import qualified Data.Git.Parser as GP

--enumGitLog :: Monad m 
--           => FilePath 
--           -> [CommandFlag]
--           -> Enumerator GitLog m b
--enumGitLog _ _ step@(Yield {})= returnI step
--enumGitLog repoPath commandOpts0 (Continue consumer) = 
--    Shell.runIO $ "git log " ++ gitOptions
--  where
--    commandOpts = ShortStat 
--                : (LogFormat "") 
--                : commandOpts0
--    gitOptions = concatMap show commandOpts

gitLogCommand :: [CommandFlag] -> String
gitLogCommand options0 = "git log " ++ options
  where
    options1 = ShortStat 
             : LogFormat "%H, %ae, %ai" 
             : NoMerges 
             : options0
    options = unwords $ map show options1
    
