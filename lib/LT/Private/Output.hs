-- | Code to produce LaTeX source to typeset lexicon entries.
--
--   Generating LaTeX presents some interesting challenges.  For typographical
--   reasons, it's better to write, e.g., @\\textit{foo,}@ than
--   @\\textit{foo},@, so the punctuation mark shares the text's formatting.
--   Parentheses are arguably an exception and should always be typeset upright
--   (see /The Elements of Typographic Style/, by Robert Bringhurs); I've chosen
--   to do it this way because it looks better with the particular fonts that
--   I'm using.
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

import LT.Text

import LT.Latin.Ast
import LT.Latin.Letter
import LT.Latin.Output
