module Main (main) where

import Test.Framework (defaultMain)

import qualified Data.Comparaptor.Tests

main :: IO ()
main = defaultMain
    [ Data.Comparaptor.Tests.tests
    ]
