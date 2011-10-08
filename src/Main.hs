{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans (liftIO)
import           Data.Enumerator (($$), (=$), run_)

import qualified Data.Enumerator.List as EL

import           App
import           Data.Git.Types
import           Data.Git.Enumerator
import           Data.Git.Enumeratee.LinesPerDay
import           System.Console.Options (CommandFlag(..), parseOpts)


main :: IO ()
main = parseOpts $ \options -> do
    (_, state) <- runAppMonad $ run_ (enum options $$ it) 
    let result = avgLinesPerDay "romanandreg@gmail.com" state
    putStrLn $ "The avg code per day for Roman is " ++ show result
  where
    enum = enumGitLog 
    it = trackLinesPerDay =$ EL.consume
