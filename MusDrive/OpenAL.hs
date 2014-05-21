module MusDrive.OpenAL
    where

import Control.Applicative
import Data.Word
import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Sound.OpenAL
import Linear.V2

data SoundList = M8  [Word8]
               | M16 [Word16]
               | S8  [V2 Word8]
               | S16 [V2 Word16]
  deriving (Eq, Show, Read)

bufferSound :: Buffer -> IO SoundList
bufferSound b = do
    (BufferData (MemoryRegion ptr size) format _) <- get $ bufferData b
    let sizei = fromIntegral size
        ptrc = castPtr ptr
    case format of
        Mono8    -> M8  <$> peekArray sizei ptrc
        Mono16   -> M16 <$> peekArray sizei ptrc
        Stereo8  -> S8  <$> peekArray sizei ptrc
        Stereo16 -> S16 <$> peekArray sizei ptrc
