{-# LANGUAGE UndecidableInstances #-}
-- |

module Instances (Binary(..)) where

import Flat (Flat(..), flat, unflat, getSize, postAligned)
import Flat.Encoder.Size (bitsToBytes)
import Data.Binary (Binary(..), put, get)
import Data.Binary.Put
import Data.Binary.Get

import Data.Word

putDynWord :: Int -> Put
putDynWord i
        | i <= fi (maxBound :: Word8)  = putWord8 0 >> putWord8 (fromIntegral i)
        | i <= fi (maxBound :: Word16) = putWord8 1 >> putWord16le (fromIntegral i)
        | i <= fi (maxBound :: Word32) = putWord8 2 >> putWord32le (fromIntegral i)
        | otherwise                    = putWord8 3 >> putWord64le (fromIntegral i)
        where
          fi :: Integral a => a -> Int
          fi = fromIntegral

getDynWord :: Get Int
getDynWord = do
      s <- getWord8
      case s of
        0 -> fromIntegral <$> getWord8
        1 -> fromIntegral <$> getWord16le
        2 -> fromIntegral <$> getWord32le
        _ -> fromIntegral <$> getWord64le

instance {-# OVERLAPPABLE #-} (Flat a) => Binary a where
  put :: (Flat a) => a -> Put
  put a = do putDynWord (bitsToBytes . getSize $ postAligned a)
             putByteString $ flat a

  get :: Flat a => Get a
  get = do
    len <- getDynWord
    bs <- getByteString len
    case unflat bs of
      Left e -> fail $ show e
      Right v -> return v
