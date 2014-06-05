{-# LANGUAGE TemplateHaskell #-}
module MusDrive.Manager
    ( runManager
    , sendCommand
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Lens
import Data.Time
import Data.Set as S

import MusDrive.Types
import MusDrive.OpenAL.Types

data ManagerState = ManagerState
    { _startTime :: UTCTime
    , _manageWaitTime :: Int
    , _bufferedSources :: Set BufferedSource
    } deriving (Show)

makeLenses ''ManagerState

defaultState :: IO ManagerState
defaultState = ManagerState <$> getCurrentTime
                            <*> waitTIme
                            <*> pure S.empty
  where
    waitTIme = return $ 10^6`div`60

type ManageAction a = StateT ManagerState IO a

runManager :: IO Manager
runManager = do
    mvar <- newEmptyMVar
    threadId <- forkIO $ do
        s <- defaultState
        runStateT (manage mvar) s
        return ()
    return $ Manager mvar threadId

manage :: (Receivable r) => r Command -> ManageAction ()
manage r = do
    cmd <- liftIO $ receive r
    currentState <- get
    case cmd of
        Just CommandTerminate -> return ()
        Just (CommandPlayTone s t) -> do
            liftIO $ do pitch s $= frequency t / s ^. baseFrequency
                        play s
            waitAndContinue
        Nothing -> waitAndContinue
  where
    waitAndContinue :: ManageAction ()
    waitAndContinue = do
        w <- use manageWaitTime
        liftIO $ threadDelay w
        manage r

sendCommand :: Manager -> Command -> IO ()
sendCommand (Manager r _) = send r
