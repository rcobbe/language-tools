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

module Latin.LetterTest(tests) where

import LT.Latin.Letter

import Test.HUnit.Base

tests = "Latin.LetterTest" ~:
  [ validLetterTests
  , parseWordTests
  , wordOrderTests
  ]

validLetterTests = [
  "bare consonant" ~: assert (validLetter 'b' NoMacron),

  "consonant with macron" ~: assertFalse (validLetter 'b' Macron),

  "vowel without macron" ~: assert (validLetter 'e' NoMacron),

  "vowel with macron" ~: assert (validLetter 'e' Macron),

  "unsupported letter, no macron" ~: assertFalse (validLetter 'j' NoMacron),

  "unsupported letter, macron" ~: assertFalse (validLetter 'w' Macron),

  "line noise" ~: assertFalse (validLetter '%' NoMacron)
  ]

parseWordTests =
  let amare = makeWord [makeLetter 'a' NoMacron,
                        makeLetter 'm' NoMacron,
                        makeLetter 'a' Macron,
                        makeLetter 'r' NoMacron,
                        makeLetter 'e' NoMacron]
  in [
    "simple word w/ underline macros" ~:
    assertNoExcept amare (parseWord "am_are"),

    "combining diacritical macro" ~:
    assertNoExcept amare (parseWord "am\x0304\&re"),

    "macron character" ~:
    assertNoExcept amare (parseWord "am\x00AF\&are"),

    "multiple macrons" ~:
    assertNoExcept amare (parseWord "am_\x00AF\x0304\&are")
  ]

-- Replace with quickcheck tests, as in Greek library?
wordOrderTests = [3 ~?= 4]

assertFalse = assert . not
