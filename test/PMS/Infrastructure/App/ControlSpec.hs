{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infrastructure.App.ControlSpec (spec) where

import Test.Hspec
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Lens
import Data.Default

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Infrastructure.App.Control as SUT
import qualified PMS.Infrastructure.DM.Type as SUT

-- |
--
data SpecContext = SpecContext {
                   _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext :: SUT.AppData
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  appDat <- SUT.defaultAppData
  return SpecContext {
           _domainDataSpecContext = domDat
         , _appDataSpecContext    = appDat
         }

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $ 
    afterAll tearDownOnce . 
      beforeWith setUp . 
        after tearDown $ run

-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] EXECUTED ONLY ONCE BEFORE ALL TESTS START."
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] EXECUTED ONLY ONCE AFTER ALL TESTS FINISH."

-- |
--
setUp :: SpecContext -> IO SpecContext
setUp ctx = do
  putStrLn "[INFO] EXECUTED BEFORE EACH TEST STARTS."

  domDat <- DM.defaultDomainData
  appDat <- SUT.defaultAppData
  return ctx {
               _domainDataSpecContext = domDat
             , _appDataSpecContext    = appDat
             }

-- |
--
tearDown :: SpecContext -> IO ()
tearDown _ = do
  putStrLn "[INFO] EXECUTED AFTER EACH TEST FINISHES."

-- |
--
run :: SpecWith SpecContext
run = do
  describe "runWithAppData" $ do
    context "when echo command issued." $ do
      it "should call callback" $ \ctx -> do 
        putStrLn "[INFO] EXECUTING THE FIRST TEST."

        let domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            cmdQ   = domDat^.DM.commandQueueDomainData
            resQ   = domDat^.DM.responseQueueDomainData
            expect = "abc"
            argDat = DM.EchoCommandData expect (callback resQ)
            args   = DM.EchoCommand argDat
            
        thId <- async $ SUT.runWithAppData appDat domDat

        STM.atomically $ STM.writeTQueue cmdQ args

        (DM.McpToolsCallResponse dat) <- STM.atomically $ STM.readTQueue resQ

        let actual = dat^.DM.jsonrpcMcpToolsCallResponseData^.DM.jsonrpcJsonRpcRequest
        actual `shouldBe` expect

        cancel thId

-- |
--
callback :: STM.TQueue DM.McpResponse -> DM.EchoCommandCallback ()
callback queue value = do
  STM.atomically $ STM.writeTQueue queue 
                 $ DM.McpToolsCallResponse
                   def {DM._jsonrpcMcpToolsCallResponseData = def {DM._jsonrpcJsonRpcRequest= value}}
