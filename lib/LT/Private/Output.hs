{-# LANGUAGE OverloadedStrings #-}

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

-- | Code to produce LaTeX source to typeset lexicon entries.
--
--   Generating LaTeX presents some interesting challenges.  For typographical
--   reasons, it's better to write, e.g., @\\textit{foo,}@ than
--   @\\textit{foo},@, so the punctuation mark shares the text's formatting.
--   Parentheses are arguably an exception and should always be typeset upright
--   (see /The Elements of Typographic Style/, by Robert Bringhurst); I've
--   chosen to do it this way because it looks better with the particular
--   fonts that I'm using.
--
--   Additionally, LaTeX tries to be smart about which periods end sentences and
--   which don't, but it doesn't always get it right, so we may need to generate
--   @\".\\\\ \"@ or @\"\\\\\@.\"@ instead of just @\".\"@.
--
--   In an earlier version of this project, I'd originally tried to use an
--   abstract representation of the output language, with constructors like
--   @Concat@ and @CommaSeparatedList@ and so forth.  One pass would map this
--   abstract representation into a \"physical\" representation that is closer
--   to the LaTeX concrete syntax; this pass would take care of moving
--   punctuation and identifying which periods need some help.  Finally, a
--   second pass would take this \"physical\" representation and generate the
--   actual LaTeX source.  Unfortunately, this didn't work out very well,
--   because it's not easily testable.  With the presence of @Concat@ in the
--   layout language, there are often many different ways to express the
--   same document, and it turns out to be difficult to write tests for these
--   passes that are only sensitive to the important differences.
--
--   So we fall back on generating LaTeX directly, but with a calling convention
--   to handle trailing punctuation.  Most functions here take a \"suffix\"
--   argument, which contains punctuation to be included within whatever
--   LaTeX formatting macro the function produces.
module LT.Private.Output where

import Prelude hiding (Word)

import LT.Text (Text)
import qualified LT.Text as Text

import LT.Latin.Ast
import LT.Latin.Letter
import LT.Latin.Output

-- | Generate LaTeX for top-level entries.  It is the caller's responsibility
--   to place the entries in the desired order.
renderTopLevelEntries :: [Entry] -> Text
renderTopLevelEntries entries =
  Text.intercalate "\n\n" (map renderTopLevelEntry entries)

-- | Generate LaTeX for a single top-level entry.
renderTopLevelEntry :: Entry -> Text
renderTopLevelEntry e =
  let guideWord = renderWordForGuide (headWord e)
  in Text.concat [
      "\\noindent\\hangpara{1em}{}\\markboth{"
    , guideWord
    , "}{"
    , guideWord
    , "}\n"
    , renderEntry e
    , "\n\\raggedright\\par"
    ]

-- | Generate LaTeX from an entry for use in the page's guide words
renderWordForGuide :: HeadWord -> Text
renderWordForGuide (Noun { nom = wd }) = renderLatin wd ""
renderWordForGuide (Verb { pp1 = wd }) = renderLatin wd ""
renderWordForGuide (Correlative w1 w2) =
  Text.concat [
      "\\textlatin{"
    , wordToLaTeX w1
    , "\\ldots{}"
    , wordToLaTeX w2
    , "}"
    ]
renderWordForGuide (Indeclinable wd) = renderLatin wd ""
renderWordForGuide (Phrase wds) =
  Text.concat [
      "\\textlatin{"
    , Text.intercalate " " (map wordToLaTeX wds)
    , "}"
    ]

-- | Generate LaTeX for an entry, either top-level or subentry.
renderEntry :: Entry -> Text
renderEntry e = undefined

-- | Generate LaTeX for a Latin word, with the supplied suffix inside the
--   Latin markup
renderLatin :: Word -> Text -> Text
renderLatin w suffix =
  Text.concat ["\\textlatin{" , wordToLaTeX w, suffix, "}"]
