{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Bits ((.|.), xor)
import Data.List (foldl')
import Data.Word (Word8)
import Data.ByteString (ByteString, unfoldr, snoc, unpack)

import Criterion.Main (defaultMain, bench, nf)

import Data.Comparaptor (SafeEq(..))

main :: IO ()
main = defaultMain
    [ bench "simpleSafeEq" $ nf (simpleSafeEq bs1) bs2
    , bench "safeEq" $ nf ((=.=) bs1) bs2
    ]
  where
    bs1 :: ByteString = unfoldr unfoldFunc 100000
    bs2 :: ByteString = snoc (unfoldr unfoldFunc 99999) 5
    unfoldFunc :: Int -> Maybe (Word8, Int)
    unfoldFunc 0 = Nothing
    unfoldFunc x = Just (fromIntegral x, x - 1)
    simpleSafeEq a b = (foldl' (.|.) 0 $ zipWith xor (unpack a) (unpack b)) == 0
