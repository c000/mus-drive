{-# LANGUAGE TypeFamilies #-}

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

data FreqBuffer freq b = FreqBuffer !freq !b
    deriving (Eq, Ord, Show)

class HasBuffer a where
    buf :: Lens' a Buffer

instance HasBuffer Buffer where
    buf = id
    {-# INLINE buf #-}

instance HasBuffer b => HasBuffer (FreqBuffer freq b) where
    buf f (FreqBuffer freq b) = buf f b <&> \b' -> FreqBuffer freq b'
    {-# INLINE buf #-}

-- | 'BundledSource' is a pair of 'Source', signal frequency and bundled 'Buffer'
data BundledSource s b = BundledSource !s !b
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

instance HasSource Source where
    source = id
    {-# INLINE source #-}

instance HasSource s => HasSource (BundledSource s b) where
    source f (BundledSource s b) = source f s <&> \s' -> BundledSource s' b
    {-# INLINE source #-}

class HasBaseFrequency a where
    type FreqType a
    baseFrequency :: Lens' a (FreqType a)

instance HasBaseFrequency (FreqBuffer freq b) where
    type FreqType (FreqBuffer freq b) = freq
    baseFrequency f (FreqBuffer freq b) = f freq <&> \freq' -> FreqBuffer freq' b
    {-# INLINE baseFrequency #-}

instance HasBaseFrequency b => HasBaseFrequency (BundledSource s b) where
    type FreqType (BundledSource s b) = FreqType b
    baseFrequency f (BundledSource s b) = baseFrequency f b <&> \b' -> BundledSource s b'
    {-# INLINE baseFrequency #-}

type BufferedSource = BundledSource Source (FreqBuffer ALfloat Buffer)

makeBundledSource :: HasBuffer b => a -> b -> IO (BundledSource Source (FreqBuffer a b))
makeBundledSource freq b = do
    s <- genObjectName
    buffer s $= Just (b ^. buf)
    return $ BundledSource s (FreqBuffer freq b)
