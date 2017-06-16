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
import Test.Utils

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
    assertNoExcept amare (parseWord "am\x0304\&are"),

    "macron character" ~:
    assertNoExcept amare (parseWord "am\x00AF\&are"),

    "underscore macron" ~:
    assertNoExcept amare (parseWord "am_are"),

    "multiple macrons" ~:
    assertNoExcept amare (parseWord "am_\x00AF\x0304\&are"),

    "precomposed macron" ~:
    assertNoExcept amare (parseWord "amāre"),

    "macron on invalid letter" ~:
    assertExcept (InvalidLetter 2) (parseWord "am_re"),

    "macron at end of word" ~:
    assertExcept (MissingLetter 3) (parseWord "abc_")
  ]

-- Replace with quickcheck tests, as in Greek library?
wordOrderTests = [
  "equal words" ~:
  (compare (parseWord "am_are") (parseWord "amāre")) ~?= EQ,

  "prefix" ~:
  (compare (parseWord "am_ab_at") (parseWord "am_ab_atis")) ~?= LT,

  "extension" ~:
  (compare (parseWord "am_ab_atis") (parseWord "am_ab_at")) ~?= GT,

  "differ only in length" ~:
  (compare (parseWord "venit") (parseWord "v_enit")) ~?= LT,

  "differ in length, but different letters" ~:
  (compare (parseWord "venit") (parseWord "v_enistis")) ~?= GT,

  "differ only in case" ~:
  (compare (parseWord "Forum") (parseWord "forum")) ~?= GT,

  "differ in case, but different letters" ~:
  (compare (parseWord "forum") (parseWord "For=o")) ~?= GT,

  "

assertFalse = assert . not
