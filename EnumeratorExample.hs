module Main where

import Data.Enumerator hiding (head)
import qualified Data.Enumerator.List as EL

sumIt :: Iteratee Int IO Int
sumIt = go 0
  where
    go accum = do
      n0 <- EL.head
      case n0 of 
        Nothing -> return accum
        Just n -> go (accum + n)

main :: IO ()
main = do
  -- returns 0
  run_ sumIt >>= print
  -- returns sum of stdin
  run_ (enumListFromTerminal $$ sumIt) >>= print
  -- returns sum of stdin plus 10
  run_ (enumListFromTerminal $$ (enumList 1 [10] $$ sumIt)) >>= print


enumListFromTerminal :: Enumerator Int IO b
enumListFromTerminal step@(Yield {}) = returnI step
enumListFromTerminal step0 = Iteratee $ do
    putStrLn "q and hit enter to stop"
    runIteratee (go step0)
  where
    go step@(Yield {}) = enumListFromTerminal step
    go step@(Continue consumer) = Iteratee $ do
      n0 <- getLine 
      if n0 == "q"
        then return step
        else do 
          let n = fst . head $ reads n0  
          runIteratee $ go $$ consumer (Chunks [n]) 
