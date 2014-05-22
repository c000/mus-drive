{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module MusDrive.Types
    where

import Control.Concurrent
import Control.Concurrent.MVar

import Data.Foldable as F
import Data.Traversable as T
import Data.Functor.Identity

type Note = Int

data GScore t n = Note n | Sequence (t (GScore t n))

instance Functor t => Functor (GScore t) where
    fmap f (Note n) = Note (f n)
    fmap f (Sequence xs) = Sequence $ fmap (fmap f) xs
    {-# INLINE fmap #-}

instance (Show n, Foldable t) => Show (GScore t n) where
    show (Note n) = " " ++ show n ++ " "
    show (Sequence xs) = "[" ++ foldMap show xs ++ "]"

type Score = GScore [] Note

testScore :: Score
testScore = Sequence [Note 1, Note 2, Sequence [Note 4, Note 5], Note 3]

data Manager = Manager (MVar Command) ThreadId

data Command = CommandTerminate

class Receivable r where
    receive :: r a -> IO a

    receiveAll :: r a -> IO [a]
    receiveAll = fmap (: []) . receive
    {-# INLINE receiveAll #-}

instance Receivable MVar where
    receive = takeMVar
    {-# INLINE receive #-}

class Sendable s where
    send :: s a -> a -> IO ()

instance Sendable MVar where
    send = putMVar
    {-# INLINE send #-}
