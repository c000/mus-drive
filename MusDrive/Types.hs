{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell #-}

module MusDrive.Types
    where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens.TH

import Data.Foldable as F
import Data.Maybe (maybeToList)
import Data.Traversable as T
import Data.Functor.Identity

import Sound.OpenAL (ALfloat)
import MusDrive.OpenAL.Types

--
-- Music types
--
newtype Tone a = Tone a deriving (Eq, Ord, Show)
makeIso ''Tone

frequencyBase :: (Floating a, Real b) => a -> Tone b -> a
frequencyBase base (Tone x) = base * 2 ** (realToFrac x / 12)
{-# SPECIALIZE frequencyBase :: ALfloat -> Tone ALfloat -> ALfloat #-}

frequency :: (Floating a, Real b) => Tone b -> a
frequency = frequencyBase 440
{-# SPECIALIZE frequency :: Tone ALfloat -> ALfloat #-}

data GScore t n = Note n | Sequence (t (GScore t n))

instance Functor t => Functor (GScore t) where
    fmap f (Note n) = Note (f n)
    fmap f (Sequence xs) = Sequence $ fmap (fmap f) xs
    {-# INLINE fmap #-}

instance (Show n, Foldable t) => Show (GScore t n) where
    show (Note n) = " " ++ show n ++ " "
    show (Sequence xs) = "[" ++ foldMap show xs ++ "]"

type Score = GScore [] (Tone ALfloat)

testScore :: Score
testScore = fmap Tone $ Sequence [Note 1, Note 2, Sequence [Note 4, Note 5], Note 3]

--
-- Managers
--
data Manager = Manager (MVar Command) ThreadId

data Command = CommandTerminate
             | CommandPlayTone BufferedSource (Tone ALfloat)

--
-- Communication channels
--
class Receivable r where
    receive :: r a -> IO (Maybe a)

    receiveAll :: r a -> IO [a]
    receiveAll = fmap maybeToList . receive
    {-# INLINE receiveAll #-}

instance Receivable MVar where
    receive = tryTakeMVar
    {-# INLINE receive #-}

class Sendable s where
    send :: s a -> a -> IO ()

instance Sendable MVar where
    send = putMVar
    {-# INLINE send #-}
