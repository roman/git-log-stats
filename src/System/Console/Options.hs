module System.Console.Options where

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
  deriving (Eq)

instance Show CommandFlag where
  show (Author name)     = "--author=" ++ name
  show (BeforeDate str)  = "--before=" ++ str
  show (AfterDate str)   = "--after=" ++ str
  show (LogFormat str)   = "--pretty=\"" ++ str ++ "\""
  show (ShortStat)       = "--shortstat"
  show (NoMerges)        = "--no-merges"
  show Help              = ""

options :: [OptDescr CommandFlag]
options =
    [ Option [] ["author"] (ReqArg Author "author")   "author of commits"
    , Option [] ["before"] (ReqArg BeforeDate "date") "before this date"
    , Option [] ["after"]  (ReqArg AfterDate "date")  "after this date"
    , Option [] ["help"]   (NoArg Help)               "display help message"
    ]

parseOpts :: ([CommandFlag] -> IO ()) -> IO ()
parseOpts fn = do
    args <- getArgs 
    case getOpt Permute options args of
      (_, _, e@(_:_)) -> do
          putStrLn "ERROR: Bad Input"
          mapM_ (putStr . ("    " ++)) e
          putStrLn ""
          putStrLn $ usageInfo "Expected Input:" options
          exitFailure
      (expected, _, _) -> 
        if Help `elem` expected 
          then do
            putStrLn $ usageInfo "Usage: Git [OPTION]" options
            exitSuccess
          else fn expected
          

