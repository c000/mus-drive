module MusDrive.Manager
    ( runManager
    , sendCommand
    ) where

import Control.Concurrent

import MusDrive.Types

runManager :: IO Manager
runManager = do
    mvar <- newEmptyMVar
    threadId <- forkIO $ manage mvar
    return $ Manager mvar threadId

manage :: (Receivable r) => r Command -> IO ()
manage r = do
    cmd <- receive r
    case cmd of
        CommandTerminate -> return ()

sendCommand :: Manager -> Command -> IO ()
sendCommand (Manager r _) = send r
