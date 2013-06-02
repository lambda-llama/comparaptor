{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.Comparaptor.Utils
    ( StrictByteString
    , inlinePerformIO
    , (&&!)
    ) where

import GHC.Base (realWorld#)
import GHC.IO (IO(IO))

import qualified Data.ByteString as StrictByteString

type StrictByteString = StrictByteString.ByteString

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

infixr 3 &&!

-- | Strict boolean @and@, regardless of is False first argument evaluates the second.
(&&!) :: Bool -> Bool -> Bool
(&&!) True !x  = x
(&&!) False !_ = False
