import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Sound.ALUT as AL
import System.IO

import MusDrive.OpenAL
import MusDrive.Types
import MusDrive.Keyboard
import MusDrive.Manager
import MusDrive.OpenAL.Types hiding (pitch)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    AL.runALUT "" [] $ \argc args -> do
        sine <- AL.createBuffer $ AL.Sine 440.0 0.0 (1/440)
        s <- makeBundledSource 440 sine
        loop s $= Looping
        manager <- runManager
        mainLoop s manager
        manager `sendCommand` CommandTerminate

mainLoop s m = do
    c <- getChar
    case c of
        '\ESC' -> return ()
        c      -> exec c
  where
    exec key = do
        result <- runMaybeT $ do
            chromatic <- key2Chromatic key
            liftIO $ m `sendCommand` CommandPlayTone s (Tone chromatic)
            return chromatic
        print result
        next
    next = mainLoop s m
