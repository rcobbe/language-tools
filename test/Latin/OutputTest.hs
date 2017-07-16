module Latin.OutputTest(tests) where

import Prelude hiding (Word)
import LT.Text (Text)
import qualified LT.Text as Text
import LT.Latin.Letter
import LT.Latin.Output

import Test.HUnit

tests = "LT.Latin.Output" ~:
  ["to Unicode" ~: makeOutputTests addTextMacron letterToUnicode wordToUnicode,
   "to LaTeX" ~: makeOutputTests addLaTeXMacron letterToLaTeX wordToLaTeX]

addTextMacron :: Text -> Text
addTextMacron = Text.cons '_'

addLaTeXMacron :: Text -> Text
addLaTeXMacron t = Text.concat [Text.fromString "\\_{", t, Text.fromString "}"]

-- | Create battery of tests for conversion to output representation, to work
--   with both Unicode and LaTeX output formats.
makeOutputTests :: (Text -> Text) -> (Letter -> Text) -> (Word -> Text)
                -> [Test]
makeOutputTests addMacron outputLetter outputWord =
  ["base letter" ~:
   outputLetter (makeLetter 'a' NoMacron) ~?= Text.singleton 'a',

   "macron" ~:
   outputLetter (makeLetter 'e' Macron) ~?= addMacron (Text.singleton 'e'),

   "capital macron" ~:
   outputLetter (makeLetter 'I' Macron) ~?= addMacron (Text.singleton 'I'),

   "word" ~:
   outputWord (literalWord "am_avist_i") ~?=
   Text.concat [Text.singleton 'a',
                Text.singleton 'm',
                addMacron (Text.singleton 'a'),
                Text.singleton 'v',
                Text.singleton 'i',
                Text.singleton 's',
                Text.singleton 't',
                addMacron (Text.singleton 'i')]]
