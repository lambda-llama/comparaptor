{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Data.Comparaptor (safeEq) where

import GHC.Base (realWorld#)
import GHC.IO (IO(IO))

import Data.Bits (Bits, (.|.), xor)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign(Ptr, castPtr)
import Foreign.C.Types (CULong(..), CChar)
import Foreign.Storable (Storable, peekElemOff)
import qualified Data.ByteString as StrictByteString

type StrictByteString = StrictByteString.ByteString

safeEq :: StrictByteString -> StrictByteString -> Bool
safeEq a b = inlinePerformIO $ unsafeUseAsCStringLen a $ \(aptr, alen) ->
    unsafeUseAsCStringLen b $ \(bptr, blen) -> case alen == blen of
        True -> safeEq' aptr bptr alen
        False -> return False

safeEq' :: Ptr CChar -> Ptr CChar -> Int -> IO Bool
safeEq' aptr bptr alen = do
    ini <- compareBytes captr cbptr inisize 0 0
    las <- compareBytes aptr bptr lassize 0 (inisize * 8)
    return $ (fromIntegral ini + las) == 0
  where
    (inisize, lassize) = alen `quotRem` 8
    captr :: Ptr CULong = castPtr aptr
    cbptr :: Ptr CULong = castPtr bptr
{-# INLINE safeEq' #-}

compareBytes :: (Bits a, Storable a) => Ptr a -> Ptr a -> Int -> a -> Int -> IO a
compareBytes aptr bptr limit = go
  where
    go acc index
        | index >= limit = return acc
        | otherwise = do
            x <- peekElemOff aptr index
            y <- peekElemOff bptr index
            go (acc .|. xor x y) (index + 1)
    {-# INLINE go #-}
{-# INLINE compareBytes #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
