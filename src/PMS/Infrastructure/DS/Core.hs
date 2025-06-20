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
import qualified Control.Concurrent as CC
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
import qualified Data.String.AnsiEscapeCodes.Strip.Text as ANSI
import qualified Data.Text.Encoding.Error as TEE
import Control.Monad

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
    go (DM.EchoCommand dat) = genEchoTask dat
    go (DM.PtyConnectCommand dat) = genPtyConnectTask dat
    go (DM.PtyTerminateCommand dat) = genPtyTerminateTask dat
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
genEchoTask :: DM.EchoCommandData -> AppContext (IOTask ())
genEchoTask dat = do
  let value = dat^.DM.valueEchoCommandData
      callback = dat^.DM.callbackEchoCommandData

  $logDebugS DM._LOGTAG $ T.pack $ "echoTask: " ++ value
  return $ echoTask value callback

  where
    -- |
    --
    echoTask :: String -> DM.EchoCommandCallback () -> IOTask ()
    echoTask value callback = do
      hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.echoTask run. " ++ value

      callback value

      hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.echoTask end."

-- |
--
genSystemTask :: DM.SystemCommandData -> AppContext (IOTask ())
genSystemTask dat = do
  scriptsDir <- view DM.scriptsDirDomainData <$> lift ask

  let nameTmp = dat^.DM.nameSystemCommandData
      callback = dat^.DM.callbackSystemCommandData
      argsBS = DM.unRawJsonByteString $ dat^.DM.argumentsSystemCommandData
  args <- liftEither $ eitherDecode $ argsBS
  
  name <- validateCommand nameTmp
  argsStr <- validateCommandArg $ args^.argumentsStringToolParams

  let cmd = scriptsDir </> name ++ ".sh" ++ " " ++ argsStr

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
      tout     = 30 * 1000 * 1000

  prompts <- view DM.promptsDomainData <$> lift ask
  pmsTMVar  <- view pmsAppData <$> ask
  procTMVar <- view processHandleAppData <$> ask
  lockTMVar <- view lockAppData <$> ask

  (cmdTmp, argsArrayTmp)  <- getCommandArgs name argsBS
  cmd <- validateCommand cmdTmp
  argsArray <- validateCommandArgs argsArrayTmp

  $logDebugS DM._LOGTAG $ T.pack $ "ptyConnectTask: cmd. " ++ cmd ++ " " ++ show argsArray

  return $ ptyConnectTask pmsTMVar procTMVar lockTMVar cmd argsArray prompts tout callback

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
               -> Int
               -> DM.PtyConnectCommandCallback ()
               -> IOTask ()
ptyConnectTask pmsTMVar procTMVar lockTMVar cmd args prompts tout callback = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyConnectTask run. " ++ cmd ++ " " ++ show args

  let env = Nothing
      -- env = Just [("TERM", "dumb")]
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

  race (expect lockTMVar pms prompts) (CC.threadDelay tout) >>= \case
    Left res  -> callback ExitSuccess (maybe "Nothing" id res) ""
    Right _ -> E.throwString "timeout occurred."

  hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.ptyConnectTask end."

  where
    -- |
    --
    errHdl :: E.SomeException -> IO ()
    errHdl e = callback (ExitFailure 1) "" (show e)


-- |
--
genPtyTerminateTask :: DM.PtyTerminateCommandData -> AppContext (IOTask ())
genPtyTerminateTask dat = do
  let callback = dat^.DM.callbackPtyTerminateCommandData
  
  pmsTMVar  <- view pmsAppData <$> ask
  procTMVar <- view processHandleAppData <$> ask

  $logDebugS DM._LOGTAG $ T.pack $ "ptyTerminateTask called. "
  return $ ptyTerminateTask pmsTMVar procTMVar callback

-- |
--
ptyTerminateTask :: STM.TMVar (Maybe Pty)
                 -> STM.TMVar (Maybe ProcessHandle)
                 -> DM.PtyTerminateCommandCallback ()
                 -> IOTask ()
ptyTerminateTask pmsTMVar procTMVar callback = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyTerminateTask run. "

  STM.atomically (STM.swapTMVar pmsTMVar Nothing) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.work.ptyTerminateTask: pty is not connected."
      callback (ExitFailure 1) "" "PTY is not connected."
    Just pms -> STM.atomically (STM.swapTMVar procTMVar Nothing) >>= \case
      Nothing -> do
        hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.work.ptyTerminateTask: invalid pty status."
        callback (ExitFailure 1) "" "invalid pty status."
      Just phandle -> do
        hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyTerminateTask closePty : "
        closePty pms
        exitCode <- waitForProcess phandle
        callback exitCode "pty teminated." ""

  hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.ptyTerminateTask end."

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
      tout = 30 * 1000 * 1000 
  prompts <- view DM.promptsDomainData <$> lift ask
  pmsTMVar  <- view pmsAppData <$> ask
  procTMVar <- view processHandleAppData <$> ask
  lockTMVar <- view lockAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS
  let args = argsDat^.argumentsStringToolParams

  $logDebugS DM._LOGTAG $ T.pack $ "ptyMessageTask: args. " ++ args
  return $ ptyMessageTask pmsTMVar procTMVar lockTMVar args prompts tout callback

-- |
--
ptyMessageTask :: STM.TMVar (Maybe Pty)
               -> STM.TMVar (Maybe ProcessHandle)
               -> STM.TMVar ()
               -> String  -- arguments line
               -> [String]  -- prompt list
               -> Int       -- timeout microsec
               -> DM.PtyMessageCommandCallback ()
               -> IOTask ()
ptyMessageTask ptyTMVar _ lockTMVar args prompts tout callback = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyMessageTask run. " ++ args

  STM.atomically (STM.readTMVar ptyTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infrastructure.DS.Core.work.ptyMessageTask: pty is not connected."
      callback (ExitFailure 1) "" "PTY is not connected."
    Just pty -> go pty

  hPutStrLn stderr "[INFO] PMS.Infrastructure.DS.Core.work.ptyMessageTask end."

  where
    -- |
    --
    errHdl :: E.SomeException -> IO ()
    errHdl e = callback (ExitFailure 1) "" (show e)

    go :: Pty -> IO ()
    go pty = do
      msg <- validateMessage args
      let cmd = TE.encodeUtf8 $ T.pack $ msg ++ _LF
      hPutStrLn stderr $ "[INFO] PMS.Infrastructure.DS.Core.work.ptyMessageTask writePty : " ++ BS.unpack cmd
      writePty pty cmd
      
      race (expect lockTMVar pty prompts) (CC.threadDelay tout) >>= \case
        Left res  -> callback ExitSuccess (maybe "Nothing" id res) ""
        Right _ -> E.throwString "timeout occurred."



-- |
--
expect :: STM.TMVar () -> Pty -> [String] -> IO (Maybe String)
expect lock pty prompts = STM.atomically (STM.tryTakeTMVar lock) >>= \case
  Nothing -> do
    hPutStrLn stderr "[INFO] expect running. skip."
    return Nothing
  Just () -> flip E.catchAny exception $ flip E.finally finalize $ do
    hPutStrLn stderr $ "[INFO] expect: " ++ show prompts
    output <- readUntilPrompt pty prompts
    let result = T.unpack (TE.decodeUtf8 output)
    return (Just result)

  where
    -- |
    --
    exception :: E.SomeException -> IO (Maybe String)
    exception e = do
      hPutStrLn stderr $ "[ERROR] expect exception: " ++ show e
      return . Just . show $ e

    -- |
    --
    finalize :: IO ()
    finalize = STM.atomically $ STM.putTMVar lock ()

-- |
--
readUntilPrompt :: Pty -> [String] -> IO BS.ByteString
readUntilPrompt pms prompts = go BS.empty
  where
    promptBsList = map BS.pack prompts

    foundPrompt acc = any (`BS.isInfixOf` acc) promptBsList

    go acc = do
      chunk <- readPty pms
      when ("\ESC[6n" `BS.isInfixOf` chunk) $ do
        E.throwString "Unsupported: Detected cursor position report request (ESC[6n)."

      let txt = ANSI.stripAnsiEscapeCodes $ TE.decodeUtf8With TEE.lenientDecode chunk
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

      let acc' = BS.append acc chunk
      if foundPrompt acc'
        then return acc'
        else go acc'

{-
readUntilPrompt :: Pty -> [String] -> IO BS.ByteString
readUntilPrompt pms prompts = go BS.empty T.empty
  where
    promptTextList = map T.pack prompts

    foundPrompt :: T.Text -> Bool
    foundPrompt txt = any (`T.isInfixOf` txt) promptTextList

    go accBs accTxt = do
      chunk <- readPty pms
      let txt = ANSI.stripAnsiEscapeCodes $ TE.decodeUtf8With TEE.lenientDecode chunk
      hPutStrLn stderr $ "[INFO] chunk:\n" ++ T.unpack txt

      when ("\ESC[6n" `BS.isInfixOf` chunk) $ do
        hPutStrLn stderr "[INFO] Detected cursor position report request, replying with ESC[1;1R"
        writePty pms (DBS.pack ([0x1B :: Word8, 0x5B :: Word8] ++ map (fromIntegral . ord) "0;0R"))

      let accBs' = BS.append accBs chunk
          accTxt' = T.append accTxt txt
      if foundPrompt accTxt'
        then return accBs'
        else go accBs' accTxt'
-}

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

