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

import Prelude hiding (Word)

import qualified Data.Char as Char

import LT.Latin.Letter

import Test.HUnit.Base
import Test.Utils
import Test.QuickCheck ((==>))
import qualified Test.QuickCheck as QC

import Latin.LetterGen

tests = "Latin.LetterTest" ~:
  [ validLetterTests
  , parseWordTests
  , wordOrderTests
  , wordOrderQCTests
  ]

validLetterTests = "validLetterTests" ~:
  ["bare consonant" ~: assert (validLetter 'b' NoMacron),

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
  in "parseWordTests" ~: [
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

wordOrderTests = "wordOrderTests" ~:
  ["equal words" ~:
   (compare (literalWord "am_are") (literalWord "amāre")) ~?= EQ,

   "prefix" ~:
   (compare (literalWord "am_ab_at") (literalWord "am_ab_atis")) ~?= LT,

   "extension" ~:
   (compare (literalWord "am_ab_atis") (literalWord "am_ab_at")) ~?= GT,

   "differ only in length" ~:
   (compare (literalWord "venit") (literalWord "v_enit")) ~?= LT,

   "differ in length, but different letters" ~:
   (compare (literalWord "venit") (literalWord "v_enistis")) ~?= GT,

   "differ only in case" ~:
   (compare (literalWord "Forum") (literalWord "forum")) ~?= LT,

   "differ in case, but different letters" ~:
   (compare (literalWord "forum") (literalWord "For_o")) ~?= GT]

wordOrderQCTests = "wordOrderQCTests" ~:
  ["ordering respects equality" ~: qc1000 propOrderRespectsEquality,
   "ordering is reflexive" ~: qc1000 propReflexive,
   "ordering is antisymmetric" ~: qc1000 propAntisymmetric,
   -- skip transitivity; too hard to generate valid input
   "ordering is total" ~: qc1000 propTotalOrder,
   "prefix always less" ~: qc1000 propPrefixLess,
   "base charater difference is most significant" ~: qc1000 propBase,
   "then macrons" ~: qc1000 propMacrons,
   "and case difference is least significant" ~: qc1000 propCase
  ]

-- | The implementations of 'Ord' and 'Eq' for 'Word' are consistent
propOrderRespectsEquality :: LikelyEqualWords -> Bool
propOrderRespectsEquality (LikelyEqualWords w1 w2) =
  (w1 == w2) == (compare w1 w2 == EQ)

-- | The (non-strict) order relation is reflexive
propReflexive :: Word -> Bool
propReflexive w = w <= w

-- | The order relation is anti-symmetric
propAntisymmetric :: Word -> Word -> Bool
propAntisymmetric w1 w2 = ((w1 <= w2) && (w2 <= w1)) == (w1 == w2)

-- | The order relation is a total order (assuming transitivity, which we
--   do not check)
propTotalOrder :: Word -> Word -> Bool
propTotalOrder w1 w2 = (w1 < w2) || (w1 == w2) || (w1 > w2)

-- | If word w1 is a prefix of w2, then w1 <= w2
propPrefixLess :: PrefixWords -> Bool
propPrefixLess (PrefixWords w1 w2) = w1 <= w2

-- | If two words have base characters that differ in more than case, then
--   that difference determines the result of comparing the words.
propBase w1 w2 =
  let getLowerBase :: Word -> [Char]
      getLowerBase w = map Char.toLower (map base (letters w))

      base1 = getLowerBase w1
      base2 = getLowerBase w2
  in (base1 /= base2) ==> (compare base1 base2 == compare w1 w2)

-- | If two words differ only in macrons and case, then the difference in
--   macrons is the difference between the words
propMacrons (LengthWords w1 w2) =
  makeOrderProperty macron w1 w2

propCase (CaseWords w1 w2) =
  makeOrderProperty (Char.isLower . base) w1 w2

-- | Constructs an order property on 'Word' values.  @makeOrderProperty
--   extractProp@ creates a property parameterized over two words, @w1@ and
--   @w2@, that says that, if @w1@ and @e2@ differ in the property selected
--   by @extractProp@, then the result of comparing the words is the same as
--   the result of comparing the property by itself.  This function assumes
--   that the words agree on all properties more significant to ordering
--   than the one selected by @extractProp@.
makeOrderProperty :: Ord a => (Letter -> a) -> Word -> Word -> QC.Property
makeOrderProperty extractProp w1 w2 =
  let props1 = map extractProp (letters w1)
      props2 = map extractProp (letters w2)
  in (props1 /= props2) ==> (compare props1 props2 == compare w1 w2)

qc1000 :: (QC.Testable a) => a -> Test
qc1000 = testQuickCheck (QC.stdArgs { QC.maxSuccess = 1000 })

assertFalse = assert . not
