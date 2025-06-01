{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infrastructure.DS.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Data.Conduit
import Control.Concurrent.Async
import qualified Data.Text as T
import Control.Monad.Except
import System.Process
import System.FilePath
import Data.Aeson
import System.Posix.Pty
import qualified Control.Exception.Safe as E
import System.Exit
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified HIE.Bios as HIE
import qualified HIE.Bios.Types as HIE
import qualified HIE.Bios.Environment as HIE
import qualified System.Directory as D


import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Infrastructure.DM.Constant
import PMS.Infrastructure.DM.Type
import PMS.Infrastructure.DS.Utility


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| cmd2task .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.Command AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.Command
    go = do
      queue <- view DM.commandQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
cmd2task :: ConduitT DM.Command (IOTask ()) AppContext ()
cmd2task = await >>= \case
  Just cmd -> flip catchError errHdl $ do
    lift (go cmd) >>= yield >> cmd2task
  Nothing -> do
    $logWarnS DM._LOGTAG "cmd2task: await returns nothing. skip."
    cmd2task

  where
    errHdl :: String -> ConduitT DM.Command (IOTask ()) AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "cmd2task: exception occurred. skip. " ++ msg
      cmd2task

    go :: DM.Command -> AppContext (IOTask ())
    go (DM.PtyConnectCommand dat) = genPtyConnectTask dat
    go (DM.PtyMessageCommand dat) = genPtyMessageTask dat
    go (DM.SystemCommand dat) = genSystemTask dat

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT (IOTask ()) Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT (IOTask ()) Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink

    go :: (IO ()) -> AppContext ()
    go t = do
      $logDebugS DM._LOGTAG "sink: start async."
      _ <- liftIOE $ async t
      $logDebugS DM._LOGTAG "sink: end async."
      return ()

---------------------------------------------------------------------------------
-- |
--
genSystemTask :: DM.SystemCommandData -> AppContext (IOTask ())
genSystemTask dat = do
  scriptsDir <- view DM.scriptsDirDomainData <$> lift ask

  let name = dat^.DM.nameSystemCommandData
      callback = dat^.DM.callbackSystemCommandData
      argsBS = DM.unRawJsonByteString $ dat^.DM.argumentsSystemCommandData
  
  args <- liftEither $ eitherDecode $ argsBS

  -- ToDo: validation
  let cmd = scriptsDir </> name ++ ".sh" ++ " " ++ (args^.argumentsStringToolParams)

  $logDebugS DM._LOGTAG $ T.pack $ "systemTask: system cmd. " ++ cmd
  return $ systemTask cmd callback

  where
    -- |
    --
    systemTask :: String -> DM.SystemCommandCallback () -> IOTask ()
    systemTask cmd callback = do
      hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.systemTask run. " ++ cmd

      (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""

      callback code out err

      hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.systemTask end."


-- |
--
genPtyConnectTask :: DM.PtyConnectCommandData -> AppContext (IOTask ())
genPtyConnectTask dat = do
  let name     = dat^.DM.namePtyConnectCommandData
      callback = dat^.DM.callbackPtyConnectCommandData
      argsBS   = DM.unRawJsonByteString $ dat^.DM.argumentsPtyConnectCommandData

  prompts <- view DM.promptsDomainData <$> lift ask
  pmsTMVar  <- view pmsAppData <$> ask
  procTMVar <- view processHandleAppData <$> ask
  lockTMVar <- view lockAppData <$> ask

  (cmd, argsArray)  <- getCommandArgs name argsBS
  
  $logDebugS DM._LOGTAG $ T.pack $ "ptyConnectTask: cmd. " ++ cmd ++ " " ++ show argsArray

  return $ ptyConnectTask pmsTMVar procTMVar lockTMVar cmd argsArray prompts callback

  where
    -- |
    --
    getCommandArgs :: String -> BL.ByteString -> AppContext (String, [String])
    getCommandArgs "pty-connect" argsBS = do
      argsDat <- liftEither $ eitherDecode $ argsBS
      let argsArray = maybe [] id (argsDat^.argumentsPtyConnectToolParams)
          cmd = argsDat^.commandPtyConnectToolParams
      return (cmd, argsArray)

    getCommandArgs "pty-bash" argsBS = do
      argsDat <- liftEither $ eitherDecode $ argsBS
      let argsArray = argsDat^.argumentsStringArrayToolParams
      return ("bash", argsArray)

    getCommandArgs "pty-ssh" argsBS = do
      argsDat <- liftEither $ eitherDecode $ argsBS
      let argsArray = argsDat^.argumentsStringArrayToolParams
      return ("ssh", argsArray)

    getCommandArgs "pty-cabal" argsBS = do
      argsDat <- liftEither $ eitherDecode $ argsBS
      let prjDir = argsDat^.projectDirPtySetCwdToolParams
          argsArray = maybe [] id (argsDat^.argumentsPtySetCwdToolParams)

      liftIOE $ D.setCurrentDirectory prjDir
      
      return ("cabal", "repl":argsArray)

    getCommandArgs "pty-stack" argsBS = do
      argsDat <- liftEither $ eitherDecode $ argsBS
      let prjDir = argsDat^.projectDirPtySetCwdToolParams
          argsArray = maybe [] id (argsDat^.argumentsPtySetCwdToolParams)

      liftIOE $ D.setCurrentDirectory prjDir
      
      return ("stack", "repl": argsArray)

    getCommandArgs "pty-ghci" argsBS = do
      argsDat <- liftEither $ eitherDecode $ argsBS
      let prjDir = argsDat^.projectDirPtyGhciToolParams
          startup = argsDat^.startupFilePtyGhciToolParams
          addArgs = maybe [] id (argsDat^.argumentsPtyGhciToolParams)

      liftIOE $ D.setCurrentDirectory prjDir
      argsArray <- liftIOE $ getGhciFlagsFromPath prjDir startup
      
      return ("ghc", argsArray ++ addArgs)

    getCommandArgs x _ = throwError $ "getCommand: unsupported command. " ++ x

-- |
--
ptyConnectTask :: STM.TMVar (Maybe Pty)
               -> STM.TMVar (Maybe ProcessHandle)
               -> STM.TMVar ()
               -> String
               -> [String]
               -> [String]
               -> DM.PtyConnectCommandCallback ()
               -> IOTask ()
ptyConnectTask pmsTMVar procTMVar lockTMVar cmd args prompts callback = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyConnectTask run. " ++ cmd ++ " " ++ show args

  let env = Nothing
      dim = (80, 24)

  (pms, procHdl) <- spawnWithPty env True cmd args dim

  STM.atomically (STM.takeTMVar pmsTMVar) >>= \case
    Just _ -> do
      hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.work.ptyConnectTask: pms is already connected."
      callback (ExitFailure 1) "" "PTY is already connected."
    Nothing -> STM.atomically $ STM.putTMVar pmsTMVar (Just pms)

  STM.atomically (STM.takeTMVar procTMVar) >>= \case
    Just _ -> do
      hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.work.ptyConnectTask: process is already connected."
      callback (ExitFailure 1) "" "Process is already connected."
    Nothing -> STM.atomically $ STM.putTMVar procTMVar (Just procHdl)

  res <- expect lockTMVar pms prompts

  callback ExitSuccess (maybe "Nothing" id res) ""

  hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.ptyConnectTask end."

  where
    -- |
    --
    errHdl :: E.SomeException -> IO ()
    errHdl e = callback (ExitFailure 1) "" (show e)


-- |
--
genPtyMessageTask :: DM.PtyMessageCommandData -> AppContext (IOTask ())
genPtyMessageTask dat = do
  let callback = dat^.DM.callbackPtyMessageCommandData
      argsBS = DM.unRawJsonByteString $ dat^.DM.argumentsPtyMessageCommandData
  
  prompts <- view DM.promptsDomainData <$> lift ask
  pmsTMVar  <- view pmsAppData <$> ask
  procTMVar <- view processHandleAppData <$> ask
  lockTMVar <- view lockAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS
  let args = argsDat^.argumentsStringToolParams

  $logDebugS DM._LOGTAG $ T.pack $ "ptyMessageTask: args. " ++ args
  return $ ptyMessageTask pmsTMVar procTMVar lockTMVar args prompts callback

-- |
--
ptyMessageTask :: STM.TMVar (Maybe Pty)
               -> STM.TMVar (Maybe ProcessHandle)
               -> STM.TMVar ()
               -> String  -- arguments line
               -> [String]  -- promt list
               -> DM.PtyMessageCommandCallback ()
               -> IOTask ()
ptyMessageTask pmsTMVar _ lockTMVar args prompts callback = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyMessageTask run. " ++ args

  STM.atomically (STM.readTMVar pmsTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.work.ptyMessageTask: pms is not connected."
      callback (ExitFailure 1) "" "PTY is not connected."
    Just pms -> do
      writePty pms $ TE.encodeUtf8 $ T.pack $ args ++ _LF
      res <- expect lockTMVar pms prompts
      callback ExitSuccess (maybe "Nothing" id res) ""

  hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.ptyMessageTask end."

  where
    -- |
    --
    errHdl :: E.SomeException -> IO ()
    errHdl e = callback (ExitFailure 1) "" (show e)


-- |
--
expect :: STM.TMVar () -> Pty -> [String] -> IO (Maybe String)
expect lock pms prompts = do
  m <- STM.atomically $ STM.tryTakeTMVar lock
  case m of
    Nothing -> return Nothing
    Just () -> do
      hPutStrLn stderr $ "[INFO] expect: " ++ show prompts
      output <- readUntilPrompt pms prompts
      STM.atomically $ STM.putTMVar lock ()
      return $ Just $ T.unpack $ TE.decodeUtf8 output

-- |
--
readUntilPrompt :: Pty -> [String] -> IO BS.ByteString
readUntilPrompt pms prompts = go BS.empty
  where
    promptBsList = map BS.pack prompts

    foundPrompt acc = any (`BS.isInfixOf` acc) promptBsList

    go acc = do
      chunk <- readPty pms
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack (TE.decodeUtf8 chunk)
      let acc' = BS.append acc chunk
      if foundPrompt acc'
        then return acc'
        else go acc'


-- |
-- 
--
getGhciFlagsFromPath :: FilePath -> Maybe FilePath -> IO [String]
getGhciFlagsFromPath prjDir startup = do
  let cwd = prjDir
  startupFile <- case startup of
    Just path -> do
      exists <- D.doesFileExist path
      return $ if exists then path else cwd </> "Dummy.hs"
    Nothing -> return $ cwd </> "Dummy.hs"

  hPutStrLn stderr $ "[INFO] getGhciFlagsFromPath: cwd: " ++ cwd
  hPutStrLn stderr $ "[INFO] getGhciFlagsFromPath: startupFile: " ++ startupFile

  explicitCradle <- HIE.findCradle startupFile
  cradle <- maybe (HIE.loadImplicitCradle mempty startupFile)
                  (HIE.loadCradle mempty) explicitCradle

  libdirResult <- HIE.getRuntimeGhcLibDir cradle
  libdir <- case libdirResult of
    HIE.CradleSuccess path -> return path
    HIE.CradleNone -> do
      hPutStrLn stderr $ "[WARN] No cradle could be found (CradleNone)"
      return ""
    HIE.CradleFail e -> do
      hPutStrLn stderr $ "[WARN] Failed to get runtime GHC libdir: " ++ show e
      return ""

  compOptsResult <- D.withCurrentDirectory cwd $
    HIE.getCompilerOptions startupFile HIE.LoadFile cradle

  flags <- case compOptsResult of
    HIE.CradleSuccess (HIE.ComponentOptions {HIE.componentOptions = fs}) -> return fs
    HIE.CradleNone -> do
      hPutStrLn stderr $ "[WARN] No cradle could be found (CradleNone)"
      return []
    HIE.CradleFail e -> do
      hPutStrLn stderr $ "[WARN] Failed to get compiler options using hie-bios cradle: " ++ show e
      return []

  return $ ["--interactive", "-fwrite-if-simplified-core"] ++
           (if null libdir then [] else ["-B" ++ libdir]) ++ flags

