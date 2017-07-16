module LT.Latin.Output( letterToUnicode
                      , wordToUnicode
                      , letterToLaTeX
                      , wordToLaTeX
                      )
where

import Prelude hiding (Word)
import Data.Textual (Textual)
import qualified Data.Textual as Textual

import LT.Latin.Letter

-- | Translates a 'Letter' to input syntax.
letterToUnicode :: Textual a => Letter -> a
letterToUnicode ltr =
  let baseText = Textual.singleton (base ltr)
  in if macron ltr == Macron
     then Textual.cons '_' baseText
     else baseText

-- | Translates a 'Word' to input syntax.
wordToUnicode :: Textual a => Word -> a
wordToUnicode wd =
  Textual.concat (map letterToUnicode (letters wd))

-- | Translates a 'Letter' to LaTeX syntax.
letterToLaTeX :: Textual a => Letter -> a
letterToLaTeX ltr =
  let baseText = Textual.singleton (base ltr)
  in if macron ltr == Macron
     then Textual.concat [ Textual.fromString "\\_{"
                         , baseText
                         , Textual.singleton '}' ]
     else baseText

wordToLaTeX wd =
  Textual.concat (map letterToLaTeX (letters wd))
