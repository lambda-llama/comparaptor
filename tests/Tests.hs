module Main (main) where

import Test.Framework (defaultMain)

import qualified Data.Comparaptor.Eq.Tests
import qualified Data.Comparaptor.Ord.Tests

main :: IO ()
main = defaultMain
    [ Data.Comparaptor.Eq.Tests.tests
    , Data.Comparaptor.Ord.Tests.tests
    ]
