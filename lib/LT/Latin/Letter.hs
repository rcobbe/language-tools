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

-- | Representation of Latin letters and words.  We need to support the
--   following operations:
--
--   1. Sorting entries into lexicographic order, where "lexicographic" is to
--      be taken quite literally: the order in which the entries appear in the
--      lexicon output.
--
--   1. Adding and removing macrons caused by morphological changes (adding
--      an ending beginning in -nt or -ns, for instance)
--
--   1. Parsing input and generating LaTeX
--
--   We also need to support some notion of capitalization, although it's
--   probably sufficient to allow capitalization only on the first letter
--   of a word, rather than general capitalization.
--
--   Modifying length due to morphological changes is easier given an
--   abstract representation of letters (@data Letter = A VowelLength | B |
--   C | ...@), because it makes diphthongs easily apparent.  However, this
--   representation complicates the implementation of ordering, for much
--   the same reason.
--
--   I'd like to stick to a single representation, if possible.  If we make the
--   assumption that we never have internal vowel hiatus between, say, A and E
--   (or any other pair of vowels that also constitutes a diphthong), then
--   we can probably get by with representing letters as a 'Char' plus a macron.
--   Morphological construction gets a little tricky, but 'Data.Sequence.Seq'
--   makes this fairly easy.  If it turns out that A/E internal hiatus /is/
--   possible, then we'll need to revisit this.
module LT.Latin.Letter
  ( Letter
  , base
  , macron
  , Macron(..)
  , validLetter
  , makeLetter
  , Word
  , letters
  , makeWord
  , parseWord
  , literalWord
) where

import Prelude hiding (Word)

import qualified Control.Monad as CM
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Function (on)

-- | Represents a single Latin letter, with macron.  We represent macrons only
--   for output representation and not for vowel length, as length is not
--   otherwise significant (unlike Greek).
data Letter = Letter { base :: Char, macron :: Macron }
  deriving (Eq, Show)

data Macron = NoMacron | Macron
  deriving (Eq, Ord, Show)

-- | Represents a Latin word
newtype Word = Word { letters :: [Letter] }
  deriving (Eq, Show)

instance Ord Word where
  -- | Compares two Latin words.  The base letters are most significant,
  --   followed by macrons, followed by case.
  compare (Word letters1) (Word letters2) =
    compareSeries
      [listCompare (compare `on` (Char.toLower . base)) letters1 letters2,
       listCompare (compare `on` macron) letters1 letters2,
       listCompare (compare `on` base) letters1 letters2]

-- | Compare two lists lexicographically, using the supplied function to
--   compare elements.
listCompare :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
listCompare compare = loop
  where loop []     [] = EQ
        loop []     _  = LT
        loop _      [] = GT
        loop (x:xs) (y:ys) =
          case compare x y of
            LT -> LT
            EQ -> loop xs ys
            GT -> GT

-- | Returns the result a series of comparisons; earlier comparisons are
--   more significant than later ones.
compareSeries :: [Ordering] -> Ordering
compareSeries = List.foldl' update EQ
 where update EQ x    = x
       update accum _ = accum

-- | Constructs a letter
makeLetter :: Char -> Macron -> Letter
makeLetter base macron =
  if validLetter base macron
  then Letter base macron
  else error (List.intercalate " " ["makeLetter: invalid combination:",
                                    show base,
                                    show macron])

-- | Recognizes valid combinations of base characters and macrons
validLetter :: Char -> Macron -> Bool
validLetter base NoMacron = isLatinLetter base
validLetter base Macron = isLatinVowel base

-- | Recognizes valid Latin letters.  Disallow J and W, but allow V -- somewhat
--   inconsistent, but it has long been the standard for printing Latin in
--   English-speaking countries.  (Oxford University Press is starting to use U
--   instead of V globally, however.)
isLatinLetter :: Char -> Bool
isLatinLetter c =
  let c' = Char.toLower c
  in 'a' <= c' && c' <= 'z' && c' /= 'j' && c' /= 'w'

-- | Recognize Latin vowels.
isLatinVowel c =
  let c' = Char.toLower c
  in c' == 'a' || c' == 'e' || c' == 'i' || c' == 'o' || c' == 'u' || c' == 'y'

-- | Constructs a Word
makeWord :: [Letter] -> Word
makeWord [] = error "makeWord: argument cannot be empty"
makeWord letters = Word letters

-- | Errors that can arise during parsing
data ParseError = EmptyInput
                | InternalError { offset :: !Int, msg :: !String }
                  -- ^ Internal error
                | MissingLetter { offset :: !Int }
                  -- ^ Macron with no following letter
                | InvalidLetter { offset :: !Int }
  deriving (Eq, Show)

-- | Parse a single word, which must extend to the end of the input
parseWord :: String -> Except ParseError Word
parseWord src =
  do CM.when (null src) (throwE EmptyInput)
     letters <- wordLoop 0 src
     return $ Word letters

-- | Parse a single word but signal error if the parse fails.  To be used only
--   with literal strings in the program source; use 'parseWord' for user
--   input.
literalWord :: String -> Word
literalWord src =
  case runExcept (parseWord src) of
    Left EmptyInput -> error "literalWord: empty input"
    Left (InternalError offset msg) ->
      error (concat ["literalWord: internal parser error at offset ",
                     show offset,
                     ": ",
                     msg])
    Left (MissingLetter offset) ->
      error (concat ["literalWord: macron with no following letter at offset ",
                     show offset])
    Left (InvalidLetter offset) ->
      error ("literalWord: invalid letter at offset " ++ show offset)
    Right w -> w

-- | Main loop for parsing a word; the first argument is the offset of the
--   current letter (equivalently, the number of letters previoulsy parsed).
wordLoop :: Int -> String -> Except ParseError [Letter]
wordLoop _     []  = return []
wordLoop index src =
  do (letter, rest) <- parseLetter index src
     restResult <- wordLoop (index + 1) rest
     return $ letter : restResult

-- | Parse a single letter; the first argument is the letter offset of the
--   input.  Does not attempt to guard against putting a macron on one of the
--   letters in a diphthong.
parseLetter :: Int -> String -> Except ParseError (Letter, String)
parseLetter index [] =
  throwE $ InternalError index "parseLetter: empty input"
parseLetter index src =
  do let (macrons, rest) = span isSrcMacron src
     CM.when (null rest) (throwE $ MissingLetter index)
     let m = if null macrons then NoMacron else Macron
         base = head rest
     if validLetter base m
     then return (makeLetter base m, tail rest)
     else throwE $ InvalidLetter index

-- | Recognize the ways in which a macron can be expressed in the input.
--   Allow underscores, macron characters, and combining macrons.
isSrcMacron c =
  c == '_' || c == '\x00AF' || c == '\x0304'
