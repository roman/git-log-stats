module System.Console.Options where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt

data CommandFlag 
  = Author     String
  | BeforeDate String
  | AfterDate  String
  | LogFormat  String
  | ShortStat
  | NoMerges

instance Show CommandFlag where
  show (Author name)     = "--author=" ++ name
  show (BeforeDate str)  = "--before=" ++ str
  show (AfterDate str)   = "--after=" ++ str
  show (LogFormat str)   = "--pretty=\"" ++ str ++ "\""
  show (ShortStat)       = "--shortstat"
  show (NoMerges)        = "--no-merges"

options :: [OptDescr CommandFlag]
options =
    [ Option [] ["author"] (ReqArg Author "author")   "author of commits"
    , Option [] ["before"] (ReqArg BeforeDate "date") "before this date"
    , Option [] ["after"]  (ReqArg AfterDate "date")  "after this date"
    ]

parseOpts :: ([CommandFlag] -> IO ()) -> IO ()
parseOpts fn = do
    args <- getArgs 
    case getOpt Permute options args of
      ([], [], []) -> do 
          putStrLn $ usageInfo "Expecting Input:" options
          exitFailure
      (_, _, e@(_:_)) -> do
          putStrLn "ERROR: Bad Input"
          mapM_ (putStr . ("    " ++)) e
          putStrLn ""
          putStrLn $ usageInfo "Expected Input:" options
          exitFailure
      (expected, _, _) -> fn expected
          

