module Data.Git.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)
import Data.Attoparsec.Char8


import Data.Git.Types

isNewline c = c == '\n'
tillComma = takeWhile (/= ',')
tillNewline = takeWhile (not . isNewline)

pLogEntry repoPath =
    GitCommit <$> pure repoPath
              <*> (skipSpace *>
                   tillComma <*
                   char ',')
              <*> (skipSpace *>
                   tillComma <*
                   char ',')
              <*> (skipSpace *>
                   tillNewline)
              <*> (skipSpace *>
                   tillComma *>
                   char ','  *>
                   skipSpace *>
                   decimal)
              <*> (tillComma *>
                   char ','  *>
                   skipSpace *>
                   decimal   <*
                   tillNewline)

parse repoPath = many (pLogEntry repoPath)
