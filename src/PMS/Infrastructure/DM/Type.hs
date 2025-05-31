{-# LANGUAGE TemplateHaskell #-}

module PMS.Infrastructure.DM.Type where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import System.Posix.Pty
import System.Process
import Data.Default
import qualified Control.Concurrent.STM as STM
import Data.Aeson.TH

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.TH as DM


data AppData = AppData {
               _pmsAppData :: STM.TMVar (Maybe Pty)
             , _processHandleAppData :: STM.TMVar (Maybe ProcessHandle) 
             , _lockAppData :: STM.TMVar ()
             }

makeLenses ''AppData

defaultAppData :: IO AppData
defaultAppData = do
  pmsVar  <- STM.newTMVarIO Nothing
  procVar <- STM.newTMVarIO Nothing
  lock    <- STM.newTMVarIO ()
  return AppData {
           _pmsAppData = pmsVar
         , _processHandleAppData = procVar
         , _lockAppData = lock
         }

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))

-- |
--
type IOTask = IO


--------------------------------------------------------------------------------------------
-- |
--
data StringToolParams =
  StringToolParams {
    _argumentsStringToolParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "StringToolParams", omitNothingFields = True} ''StringToolParams)
makeLenses ''StringToolParams

instance Default StringToolParams where
  def = StringToolParams {
        _argumentsStringToolParams = def
      }

-- |
--
data StringArrayToolParams =
  StringArrayToolParams {
    _argumentsStringArrayToolParams :: [String]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "StringArrayToolParams", omitNothingFields = True} ''StringArrayToolParams)
makeLenses ''StringArrayToolParams

instance Default StringArrayToolParams where
  def = StringArrayToolParams {
        _argumentsStringArrayToolParams = def
      }

-- |
--
data PtyConnectToolParams =
  PtyConnectToolParams {
    _commandPtyConnectToolParams   :: String
  , _argumentsPtyConnectToolParams :: Maybe [String]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "PtyConnectToolParams", omitNothingFields = True} ''PtyConnectToolParams)
makeLenses ''PtyConnectToolParams

instance Default PtyConnectToolParams where
  def = PtyConnectToolParams {
        _commandPtyConnectToolParams   = def
      , _argumentsPtyConnectToolParams = def
      }

-- |
--
data PtyGhciToolParams =
  PtyGhciToolParams {
    _projectDirPtyGhciToolParams  :: String
  , _startupFilePtyGhciToolParams :: Maybe String
  , _argumentsPtyGhciToolParams   :: Maybe [String]
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "PtyGhciToolParams", omitNothingFields = True} ''PtyGhciToolParams)
makeLenses ''PtyGhciToolParams

instance Default PtyGhciToolParams where
  def = PtyGhciToolParams {
        _projectDirPtyGhciToolParams  = def
      , _startupFilePtyGhciToolParams = def
      , _argumentsPtyGhciToolParams   = def
      }
