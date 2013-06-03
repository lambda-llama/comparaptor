{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

module Data.Comparaptor.Ord.Tests (tests) where

#define TEST(TYPE) testProperty "safeCompare TYPE" (testOrd :: TYPE -> TYPE -> Bool)

import qualified Data.ByteString as StrictByteString

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))

import Data.Comparaptor.Ord (SafeOrd(..))

type StrictByteString = StrictByteString.ByteString

instance Arbitrary StrictByteString where
    arbitrary = fmap StrictByteString.pack arbitrary

testOrd :: (SafeOrd a, Ord a) => a -> a -> Bool
testOrd a b = expected == res
  where
    expected = a `compare` b
    res = a `safeCompare` b

tests :: Test
tests = testGroup "Data.Comparaptor.Ord.Tests"
    [ TEST(StrictByteString)
    ]
