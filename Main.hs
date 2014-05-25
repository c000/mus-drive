import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Sound.ALUT
import System.IO

import MusDrive.OpenAL
import MusDrive.Types
import MusDrive.Keyboard
import MusDrive.Manager

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    runALUT "" [] $ \argc args -> do
        sine <- createBuffer $ Sine 440.0 0.0 (1/440)
        source <- genObjectName
        d <- get $ bufferData sine
        print d
        loopingMode source $= Looping
        buffer source $= Just sine
        tid <- forkIO myForkFunction
        manager <- runManager
        mainLoop source
        manager `sendCommand` CommandTerminate
        killThread tid

myForkFunction = bracket_ (putStrLn "Begin") (putStrLn "End") loop
  where
    loop = threadDelay (10^6`div`60) >> loop

mainLoop source = do
    c <- getChar
    case c of
        '\ESC' -> return ()
        c      -> exec c
  where
    exec key = do
        result <- runMaybeT $ do
            chromatic <- key2Chromatic key
            liftIO $ pitch source $= 2 ** (chromatic / 12)
            liftIO $ play [source]
            return chromatic
        print result
        next
    next = mainLoop source
