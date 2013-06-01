{-# LANGUAGE TypeSynonymInstances #-}

module Data.Comparaptor.Eq.Tests (tests) where

import qualified Data.ByteString as StrictByteString

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))

import Data.Comparaptor.Eq (SafeEq(..))

type StrictByteString = StrictByteString.ByteString

instance Arbitrary StrictByteString where
    arbitrary = fmap StrictByteString.pack arbitrary

testEq :: (SafeEq a, Eq a) => a -> a -> Bool
testEq a b = expected == res
  where
    expected = a == b
    res = a =.= b

tests :: Test
tests = testGroup "Data.Comparaptor.Eq.Tests"
    [ testProperty "test Eq" (testEq :: StrictByteString -> StrictByteString -> Bool)
    ]
