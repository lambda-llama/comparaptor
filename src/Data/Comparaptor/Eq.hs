{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.Comparaptor.Eq (SafeEq(..)) where

import GHC.Base (realWorld#)
import GHC.IO (IO(IO))

import Data.Bits (Bits, (.|.), xor)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign(Ptr, castPtr)
import Foreign.C.Types (CULong(..), CChar)
import Foreign.Storable (Storable, peekElemOff)
import qualified Data.ByteString as StrictByteString

type StrictByteString = StrictByteString.ByteString

------------------------------------------------------------------------------
-- * Class

-- | This class provide '(=.=)' equality function that should compare two 'a'
-- without timing attacks vulnerabilities.
class SafeEq a where
    (=.=) :: a -> a -> Bool

------------------------------------------------------------------------------
-- * Instances

------------------------------------------------------------------------------
-- ** Simple instances

instance SafeEq () where
    () =.= () = True
    {-# INLINE (=.=) #-}

instance SafeEq Bool where
    True =.= True   = True
    False =.= False = True
    _ =.= _         = False
    {-# INLINE (=.=) #-}

------------------------------------------------------------------------------
-- ** Tuple instances

instance (SafeEq a, SafeEq b) => SafeEq (a, b) where
    (a, b) =.= (a', b') = a =.= a' &&! b =.= b'
    {-# INLINE (=.=) #-}

instance (SafeEq a, SafeEq b, SafeEq c) => SafeEq (a, b, c) where
    (a, b, c) =.= (a', b', c') = a =.= a' &&! b =.= b' &&! c =.= c'
    {-# INLINE (=.=) #-}

------------------------------------------------------------------------------
-- ** Not so simple instances

-- | O(1) for 'StrictByteString.ByteString's with different length and O(n) for
-- 'StrictByteString.ByteString's with same length.
instance SafeEq StrictByteString where
    a =.= b = inlinePerformIO $ unsafeUseAsCStringLen a $ \(aptr, alen) ->
        unsafeUseAsCStringLen b $ \(bptr, blen) -> case alen == blen of
            True -> safeEq' aptr bptr alen
            False -> return False
    {-# INLINE (=.=) #-}

------------------------------------------------------------------------------
-- * Helper functions

-- | Compare two byte arrays with given length. Assume that byte arrays
-- has same length otherwise segmentation fault is quite possible.
--
-- This function works in two different stages:
--
--   * Compare first part of bytestring with 'CULong', so we need to
--     make much less readings from memory.
--   * Compare rest with 'CChar'
safeEq' :: Ptr CChar -- ^ Pointer to first byte array
        -> Ptr CChar -- ^ Pointer to second byte array
        -> Int       -- ^ Byte arrays size
        -> IO Bool   -- ^ Is byte arrays equals
safeEq' aptr bptr alen = do
    ini <- compareBytes captr cbptr inisize 0
    las <- compareBytes aptr bptr lassize (inisize * 8)
    return $ ini && las
  where
    (inisize, lassize) = alen `quotRem` 8
    captr :: Ptr CULong = castPtr aptr
    cbptr :: Ptr CULong = castPtr bptr
{-# INLINE safeEq' #-}

compareBytes :: (Num a, Bits a, Storable a)
             => Ptr a   -- ^ Pointer to first byte array
             -> Ptr a   -- ^ Pointer to second byte array
             -> Int     -- ^ Maxumum number of readings 'a' in byte arrays
             -> Int     -- ^ Offset in numbers of 'a' in byte arrays
             -> IO Bool -- ^ Is byte arrays equals
compareBytes aptr bptr limit = go 0
  where
    go acc index
        | index >= limit = return $ acc == 0
        | otherwise = do
            x <- peekElemOff aptr index
            y <- peekElemOff bptr index
            go (acc .|. xor x y) (index + 1)
    {-# INLINE go #-}
{-# INLINE compareBytes #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

infixr 3 &&!

(&&!) :: Bool -> Bool -> Bool
(&&!) True !x  = x
(&&!) False !_ = False
