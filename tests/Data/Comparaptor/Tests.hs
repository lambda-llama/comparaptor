{-# LANGUAGE TypeSynonymInstances #-}

module Data.Comparaptor.Tests (tests) where

import qualified Data.ByteString as StrictByteString

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))

import Data.Comparaptor (safeEq)

type StrictByteString = StrictByteString.ByteString

instance Arbitrary StrictByteString where
    arbitrary = fmap StrictByteString.pack arbitrary

testEq :: StrictByteString -> StrictByteString -> Bool
testEq a b = expected == res
  where
    expected = a == b
    res = a `safeEq` b

tests :: Test
tests = testGroup "Data.Comparaptor.Tests"
    [ testProperty "test Eq different" testEq
    ]
