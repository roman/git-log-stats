module Main where

import Data.Attoparsec as P

import qualified Data.ByteString as B
import           Data.Git.Types
import qualified Data.Git.Parser as GP
import           Data.Git.Enumerator
import           System.Console.Options (CommandFlag(..), parseOpts)

main :: IO ()
--main = parseOpts $ putStrLn . gitLogCommand 
main = do
  str <- B.getContents
  print $ P.parseOnly GP.parse str
