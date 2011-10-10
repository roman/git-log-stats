{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import           Data.List (foldl1')
import           Data.Enumerator (
    Enumerator
  , (>==>)
  , ($$)
  , (=$)
  , run_
  )
import qualified Data.Enumerator.List as EL
import           Data.Maybe (fromMaybe)

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

haveForbiddenPaths :: [CommandFlag] -> Maybe [ByteString]
haveForbiddenPaths = safeHead . filter fn
  where
    fn (ForbiddenPaths {}) = True
    fn _ = False 
    safeHead [] = Nothing
    safeHead [ForbiddenPaths xs] = Just xs


main :: IO ()
main = parseOpts $ \repos options -> do
    let enum = enumRepos options repos
    let forbiddenPaths = fromMaybe [] $ haveForbiddenPaths options 
    let it = trackLinesPerDay forbiddenPaths
           =$ EL.consume
    (_, state) <- runAppMonad $ run_ (enum $$ it)
    let result = avgLinesPerDay "Roman Gonzalez" state
    putStrLn $ "The avg code per day for Roman is " ++ show result
