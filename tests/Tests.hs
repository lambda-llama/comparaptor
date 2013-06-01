module Main (main) where

import Test.Framework (defaultMain)

import qualified Data.Comparaptor.Eq.Tests

main :: IO ()
main = defaultMain
    [ Data.Comparaptor.Eq.Tests.tests
    ]
