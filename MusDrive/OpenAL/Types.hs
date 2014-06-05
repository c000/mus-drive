module MusDrive.OpenAL.Types
    ( BundledSource (..)
    , HasSource (..)
    , HasBuffer (..)
    , baseFrequency
    , BufferedSource
    , makeBundledSource
    , ($=)
    , ($~)
    , ($~!)
    , ($=!)
    , stateGet
    , LoopingMode (..)
    ) where

import Control.Lens

import Sound.OpenAL as AL

stateGet :: HasGetter g => g a -> IO a
stateGet = AL.get

-- | 'BundledSource' is a pair of 'Source', signal frequency and bundled 'Buffer'
data BundledSource freq b = BundledSource !Source !freq !b
    deriving (Eq, Ord, Show)

class HasSource a where
    source :: Lens' a Source
    pitch :: a -> StateVar ALfloat
    pitch s = AL.pitch (s ^. source)
    {-# INLINE pitch #-}
    loop :: a -> StateVar LoopingMode
    loop s = AL.loopingMode (s ^. source)
    {-# INLINE loop #-}
    play, pause, stop, rewind :: a -> IO ()
    play   s = AL.play   [s ^. source]
    {-# INLINE play #-}
    pause  s = AL.pause  [s ^. source]
    {-# INLINE pause #-}
    stop   s = AL.stop   [s ^. source]
    {-# INLINE stop #-}
    rewind s = AL.rewind [s ^. source]
    {-# INLINE rewind #-}

instance HasSource (BundledSource f b) where
    source f (BundledSource s freq b) = f s <&> \s' -> BundledSource s' freq b
    {-# INLINE source #-}

baseFrequency :: Lens' (BundledSource freq b) freq
baseFrequency f (BundledSource s freq b) = f freq <&> \freq' -> BundledSource s freq' b

class HasBuffer a where
    buf :: Lens' a Buffer

instance HasBuffer Buffer where
    buf f = f
    {-# INLINE buf #-}

type BufferedSource = BundledSource ALfloat Buffer

makeBundledSource :: HasBuffer b => a -> b -> IO (BundledSource a b)
makeBundledSource freq b = do
    s <- genObjectName
    buffer s $= Just (b ^. buf)
    return $ BundledSource s freq b
