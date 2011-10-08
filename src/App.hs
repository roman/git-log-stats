{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module App where

-------------------------------------------------------------------------------

import Control.Monad.State (StateT, runStateT, modify)
import Control.Monad.Trans (MonadIO)
import Data.Lens.Common (getL, modL)
import Data.Lens.Template (makeLenses)
import Data.Map (Map)
import Data.Monoid (Monoid(..), Sum(..))
import Data.Set (Set)

import qualified Data.Map as Map

-------------------------------------------------------------------------------

import Data.Git.Types
import Data.Git.Enumeratee.LinesPerDay

-------------------------------------------------------------------------------

type DateMap = Map CommitDate (Sum Integer)
type CommitMap = Map CommitAuthor DateMap

data AppState
  = AppState {
    _commitMap  :: CommitMap
  }
  deriving (Show)

newtype AppMonad m b
  = AppMonad { 
    _fromAppMonad :: StateT AppState m b
  }
  deriving (Monad, MonadIO)


makeLenses [''AppState, ''AppMonad]

-------------------------------------------------------------------------------

instance Monad m => HasLinesPerDay (AppMonad m) where
  registerLPD author date n = AppMonad $
      modify (modL commitMap appendAuthor) 
    where
      appendAuthor = Map.alter appendDate author 
      appendDate Nothing = Just $ Map.fromList [(date, Sum n)]
      appendDate (Just datem) = Just $ Map.alter appendSum date datem
      appendSum Nothing = Just $ Sum n
      appendSum (Just n') = Just $ Sum n `mappend` n'


-------------------------------------------------------------------------------

emptyAppState :: AppState
emptyAppState = AppState Map.empty

runAppMonad :: Monad m => AppMonad m a -> m (a, AppState)
runAppMonad (AppMonad app) = runStateT app emptyAppState

avgLinesPerDay :: CommitAuthor -> AppState -> Maybe Double
avgLinesPerDay author state =  
    processSumDivByDay `fmap` Map.lookup author (getL commitMap state)
  where
    processSumDivByDay :: DateMap -> Double
    processSumDivByDay days =
      let nom = getSum (Map.fold mappend mempty days)
          denom = Map.size days
      in (fromIntegral nom) / (fromIntegral denom)

-------------------------------------------------------------------------------

