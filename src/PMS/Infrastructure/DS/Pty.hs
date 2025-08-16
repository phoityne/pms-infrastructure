{-# LANGUAGE OverloadedStrings #-}

module PMS.Infrastructure.DS.Pty where

import System.Posix.Pty
import qualified Data.ByteString as BS
import qualified Control.Exception as E
import System.Posix.IO (fdRead)
import System.Posix.IO
import System.Posix.Types
import System.Posix.Select
import Foreign.C.Types (CInt(..))

import Control.Exception (bracket)
import Control.Concurrent.MVar (withMVar)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types ()
import Foreign.Marshal.Array (newArray0)
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.IO.Error (throwErrnoIfMinus1Retry)
import GHC.IO.Handle.Internals (mkProcessHandle)
import System.Process.Internals (runInteractiveProcess_lock, fromBool)
import System.Posix.Pty.ForkExec (fork_exec_with_pty)
import System.Posix.Types (Fd)


-- |
-- https://hackage.haskell.org/package/posix-pty-0.2.2/docs/src/System.Posix.Pty.html#spawnWithPty
-- 
spawnWithPtyFd :: Maybe [(String, String)]    -- ^ Optional environment for the
                                            --   new process.
             -> Bool                        -- ^ Search for the executable in
                                            --   PATH?
             -> FilePath                    -- ^ Program's name.
             -> [String]                    -- ^ Command line arguments for the
                                            --   program.
             -> (Int, Int)                  -- ^ Initial dimensions for the
                                            --   pseudo terminal.
             -> IO (Pty, Fd, ProcessHandle)
spawnWithPtyFd env' (fromBool -> search) path' argv' (x, y) = do
    bracket allocStrings cleanupStrings $ \(path, argvList, envList) -> do
        let allocLists = do
                argv <- newArray0 nullPtr (path : argvList)
                env <- case envList of
                        [] -> return nullPtr
                        _ -> newArray0 nullPtr envList
                return (argv, env)

            cleanupLists (argv, env) = free argv >> free env

        bracket allocLists cleanupLists $ \(argv, env) -> do
            alloca $ \pidPtr -> do
                fd <- throwErrnoIfMinus1Retry "failed to fork or open pty" $
                        withMVar runInteractiveProcess_lock $ \_ ->
                          fork_exec_with_pty x y search path argv env pidPtr

                mpty <- createPty fd
                case mpty of
                    Nothing  -> error "createPty: failed to wrap fd"
                    Just pty -> do
                        pid <- peek pidPtr
                        handle <- mkProcessHandle (fromIntegral pid) True
                        return (pty, fd, handle)

  where
    fuse :: (String, String) -> IO CString
    fuse (key, val) = newCString (key ++ "=" ++ val)

    allocStrings :: IO (CString, [CString], [CString])
    allocStrings = do
        path <- newCString path'
        argv <- mapM newCString argv'
        env <- maybe (return []) (mapM fuse) env'
        return (path, argv, env)

    cleanupStrings :: (CString, [CString], [CString]) -> IO ()
    cleanupStrings (path, argv, env) = do
        free path
        mapM_ free argv
        mapM_ free env


-- | 
--
waitForFdInput :: Fd -> Int -> IO Bool
waitForFdInput fd timeoutMillis = do
    let sec  = timeoutMillis `div` 1000
        usec = (timeoutMillis `mod` 1000) * 1000
    (readable, _, _) <- select [fd] [] [] (Just (sec, usec))
    return (not (null readable))


-- | 
--
readPtyFd :: Pty-> Fd -> IO BS.ByteString
readPtyFd pty fd = do
    let timeoutMillis = 5000
    ready <- waitForFdInput fd timeoutMillis
    if ready
        then readPty pty
        else E.throwString "readProcFd: timeout waiting for input"

