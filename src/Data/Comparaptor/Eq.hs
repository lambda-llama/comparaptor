{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}

module Data.Comparaptor.Eq (SafeEq(..)) where

import GHC.Exts (Int(I#), Ptr(..))
import GHC.Prim (byteArrayContents#, (==#), (*#))
import GHC.Integer.GMP.Internals (Integer(S#, J#))

import Data.Bits (Bits, (.|.), xor)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign(castPtr)
import Foreign.C.Types (CULong(..), CChar)
import Foreign.Storable (Storable, peekElemOff)

import Data.Comparaptor.Utils (StrictByteString, inlinePerformIO, (&&!))

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

instance SafeEq Integer where
    (S# a) =.= (S# b) = a ==# b
    (J# _ _) =.= (S# _) = False
    (S# _) =.= (J# _ _) = False
    (J# alen a) =.= (J# blen b) = alen ==# blen &&! (inlinePerformIO $ safeEq' aptr bptr $ I# (alen *# 8#))
      where
        aaddr = byteArrayContents# a
        baddr = byteArrayContents# b
        aptr = Ptr aaddr
        bptr = Ptr baddr
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
    ini <- fmap fromIntegral $ compareBytes captr cbptr inisize 0 0
    fmap (== 0) $ compareBytes aptr bptr lassize ini (inisize * 8)
  where
    (inisize, lassize) = alen `quotRem` 8
    captr :: Ptr CULong = castPtr aptr
    cbptr :: Ptr CULong = castPtr bptr
{-# INLINE safeEq' #-}

compareBytes :: (Num a, Bits a, Storable a)
             => Ptr a   -- ^ Pointer to first byte array
             -> Ptr a   -- ^ Pointer to second byte array
             -> Int     -- ^ Maxumum number of readings 'a' in byte arrays
             -> a       -- ^ Initial value
             -> Int     -- ^ Offset in numbers of 'a' in byte arrays
             -> IO a    -- ^ 0 if byte arrays equals
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
