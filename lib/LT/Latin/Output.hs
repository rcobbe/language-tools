module LT.Latin.Output( letterToUnicode
                      , wordToUnicode
                      , letterToLaTeX
                      , wordToLaTeX
                      )
where

import Prelude hiding (Word)
import LT.Text (Text)
import qualified LT.Text as Text

import LT.Latin.Letter

-- | Translates a 'Letter' to input syntax.
letterToUnicode :: Letter -> Text
letterToUnicode ltr =
  let baseText = Text.singleton (base ltr)
  in if macron ltr == Macron
     then Text.cons '_' baseText
     else baseText

-- | Translates a 'Word' to input syntax.
wordToUnicode :: Word -> Text
wordToUnicode wd =
  Text.concat (map letterToUnicode (letters wd))

-- | Translates a 'Letter' to LaTeX syntax.
letterToLaTeX :: Letter -> Text
letterToLaTeX ltr =
  let baseText = Text.singleton (base ltr)
  in if macron ltr == Macron
     then Text.concat [ Text.fromString "\\_{"
                      , baseText
                      , Text.singleton '}' ]
     else baseText

wordToLaTeX wd =
  Text.concat (map letterToLaTeX (letters wd))
