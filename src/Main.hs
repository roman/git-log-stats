{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.List (foldl1')
import           Data.Enumerator (
    Enumerator
  , (>==>)
  , ($$)
  , (=$)
  , run_
  )
import qualified Data.Enumerator.List as EL

import           App
import           Data.Git.Types
import           Data.Git.Enumerator
import           Data.Git.Enumeratee.LinesPerDay
import           System.Console.Options (CommandFlag(..), parseOpts)


enumRepos :: MonadIO m
          => [CommandFlag]
          -> [FilePath]
          -> Enumerator GitCommit m b
enumRepos options =
    foldl1' (>==>) .
    map (enumGitLog options $)

main :: IO ()
main = parseOpts $ \repos options -> do
    let enum = enumRepos options repos
    let it   = trackLinesPerDay ["/vendor", "/dist", "/cabal-dev"] 
             =$ EL.consume
    (logList, state) <- runAppMonad $ run_ (enum $$ it)
    let result = avgLinesPerDay "Roman Gonzalez" state
    putStrLn $ "The avg code per day for Roman is " ++ show result
    --mapM_ print logList
