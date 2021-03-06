module System.Console.Options where


import Data.ByteString.Char8 (ByteString, pack)
import Data.List.Split (splitOn)

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt

data CommandFlag 
  = Author     String
  | BeforeDate String
  | AfterDate  String
  | LogFormat  String
  | ShortStat
  | NoMerges
  | Help
  | ForbiddenPaths [ByteString]
  deriving (Eq)

instance Show CommandFlag where
  show (Author name)     = "--author=" ++ name
  show (BeforeDate str)  = "--before=" ++ str
  show (AfterDate str)   = "--after=" ++ str
  show (LogFormat str)   = "--pretty=\"" ++ str ++ "\""
  show (ShortStat)       = "--numstat"
  show (NoMerges)        = "--no-merges"
  show _                 = ""

options :: [OptDescr CommandFlag]
options =
    [ Option [] ["author"]  (ReqArg Author "author")   "author of commits"
    , Option [] ["before"]  (ReqArg BeforeDate "date") "before this date"
    , Option [] ["after"]   (ReqArg AfterDate "date")  "after this date"
    , Option [] ["exclude"] (ReqArg forbiddenPaths "paths") "paths separated by commas"
    , Option [] ["help"]    (NoArg Help)               "display help message"
    ]
  where
    forbiddenPaths = ForbiddenPaths . map pack . splitOn ","

parseOpts :: ([String] -> [CommandFlag] -> IO ()) -> IO ()
parseOpts fn = do
    args <- getArgs 
    case getOpt Permute options args of
      (_, _, e@(_:_)) -> do
          putStrLn "ERROR: Bad Input"
          mapM_ (putStr . ("    " ++)) e
          putStrLn ""
          putStrLn $ usageInfo "Expected Input:" options
          exitFailure
      (expected, normal, _) -> 
        if Help `elem` expected 
          then do
            putStrLn $ usageInfo "Usage: Git [OPTION]" options
            exitSuccess
          else fn normal expected
          

