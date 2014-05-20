import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Sound.ALUT
import System.IO

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
        mainLoop source

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

key2Chromatic :: (Num a, Monad m) => Char -> m a
key2Chromatic 'z' = return (-9)
key2Chromatic 's' = return (-8)
key2Chromatic 'x' = return (-7)
key2Chromatic 'd' = return (-6)
key2Chromatic 'c' = return (-5)
key2Chromatic 'v' = return (-4)
key2Chromatic 'g' = return (-3)
key2Chromatic 'b' = return (-2)
key2Chromatic 'h' = return (-1)
key2Chromatic 'n' = return ( 0)
key2Chromatic 'j' = return ( 1)
key2Chromatic 'm' = return ( 2)
key2Chromatic ',' = return ( 3)
key2Chromatic 'l' = return ( 4)
key2Chromatic '.' = return ( 5)
key2Chromatic ';' = return ( 6)
key2Chromatic '/' = return ( 7)
key2Chromatic '\\'= return ( 8)
key2Chromatic _ = fail "Out of chromatic keyboard region"
