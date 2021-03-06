-- Copyright 2017 Richard Cobbe
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main(main) where

import Control.Monad
import System.Exit
import Test.HUnit

import qualified Latin.LetterTest
import qualified Latin.ScannerTest
import qualified Latin.ParserTest
import qualified Latin.OutputTest

main :: IO ()
main =
  do c <- runTestTT tests
     when (errors c /= 0 || failures c /= 0) exitFailure

tests =
  "Language Tools" ~:
  [ Latin.LetterTest.tests
  , Latin.ScannerTest.tests
  , Latin.ParserTest.tests
  , Latin.OutputTest.tests
  ]
