{-# LANGUAGE FlexibleContexts #-}

module MusDrive.Types
    where

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
    show (Sequence xs) = "[" ++ (foldMap show xs) ++ "]"

type Score = GScore [] Note

testScore :: Score
testScore = Sequence [Note 1, Note 2, Sequence [Note 4, Note 5], Note 3]
