{-# LANGUAGE TypeSynonymInstances #-}

module Data.Comparaptor.Ord (SafeOrd(..)) where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign(Ptr)
import Foreign.C.Types (CChar(..), CInt(..), CSize(..))

import Data.Comparaptor.Utils (StrictByteString, inlinePerformIO)

------------------------------------------------------------------------------
-- * Class

class SafeOrd a where
    safeCompare :: a -> a -> Ordering

------------------------------------------------------------------------------
-- * Instances

instance SafeOrd StrictByteString where
    a `safeCompare` b = inlinePerformIO $ unsafeUseAsCStringLen a $ \(aptr, alen) ->
        unsafeUseAsCStringLen b $ \(bptr, blen) ->
            fmap (report alen blen) $ memcmp aptr bptr $ fromIntegral $ min alen blen
      where
        report alen blen res
          | res < 0   = LT
          | res == 0  = alen `compare` blen  -- FIXME: alen `safeCompare` blen
          | otherwise = GT

foreign import ccall "comparaptor.h safe_memcmp"
    memcmp :: Ptr CChar -> Ptr CChar -> CSize -> IO CInt
