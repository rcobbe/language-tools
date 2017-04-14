module Main(main) where

import Control.Monad
import System.Exit
import Test.HUnit

import qualified Latin.ScannerTest

main :: IO ()
main =
  do c <- runTestTT tests
     when (errors c /= 0 || failures c /= 0) exitFailure

tests =
  "Language Tools" ~:
  [Latin.ScannerTest.tests]
