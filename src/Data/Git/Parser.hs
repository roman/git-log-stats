module Data.Git.Parser where

import           Prelude hiding (takeWhile)
import           Control.Applicative hiding (many)
import           Data.Attoparsec.Char8

import qualified Data.Set as Set

import           Data.Git.Types

isNewline c = c == '\n'
tillComma = takeWhile (/= ',')
tillNewline = takeWhile (not . isNewline)

pCommitFiles =
    CommitFile <$> (skipSpace *>
                    decimal)
               <*> (skipSpace *>
                    decimal)
               <*> (skipSpace *>
                    tillNewline)

pCommitEntry repoPath =
    GitCommit <$> pure repoPath
              <*> (skipSpace *>
                   tillComma <*
                   char ',')
              <*> (skipSpace *>
                   tillComma <*
                   char ',')
              <*> (skipSpace *>
                   tillNewline)
              <*> (Set.fromList <$>
                   many1 pCommitFiles)

parse repoPath = many (pCommitEntry repoPath)
