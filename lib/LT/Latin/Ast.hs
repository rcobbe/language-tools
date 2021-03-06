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

module LT.Latin.Ast where

import Prelude hiding (Word)

import Data.Map (Map)
import Data.Set (Set)

import LT.Text (Text)
import LT.Latin.Letter (Word)

-- | A single vocabulary item, with morphological information and definitions.
data Entry = Entry { entryPos :: Location
                   , headWord :: HeadWord
                   , entryNum :: Maybe Int
                     -- ^ distinguishes two entries with the same head word
                   , note :: Maybe Text
                     -- ^ additional non-definitional information about lexeme
                   , definitions :: [Definition]
                     -- ^ non-empty list of definitions
                   , subEntries :: [Entry]
                   , citations :: [Citation]
                   }
           deriving (Eq, Show)

-- XXX should entryNum be in Entry or in HeadWord?  Putting it in HeadWord
-- could make it easier for morphological generation to link back to the full
-- head word, including entryNum.  Moving it there does complicate the HeadWord
-- representation, however.

-- Definition subcategorization and syntactic information are put into a
-- general "note" field, rather than devising a statically-typed representation
-- for object case and various other objects.  Such a representation would lead
-- to a ridiculous proliferation of types.  The drawbacks are that we can't
-- format the note text according to its semantics, and we get less checking
-- of the user input for, e.g., misspelled case names.

-- | The head word of an 'Entry'.  Contains all morphological information, so
--   know we never have, e.g., a noun's morphological info with a correlative
--   head word.
data HeadWord = Noun { nom :: Word
                     , gen :: Word
                     , genders :: Set Gender
                       -- ^ must be non-empty
                     , n_overrides :: OverrideMap NounParse
                     }
              | Verb { pp1 :: Word
                     , pp2 :: Word
                     , pp3 :: Word
                     , pp4 :: Word
                     , v_overrides :: OverrideMap VerbParse
                     }
              | Correlative Word Word
              | Indeclinable Word
              | Phrase [Word]  -- nonempty list
              deriving (Eq, Show)

-- XXX are there Latin nouns with more than one gender?

-- | Single definition in a lexicon entry.  Right now, the body is unanalyzed
--   text, although we'll probably want to extend this to support contents in
--   non-Latin scripts.  I considered adding a field for notes here, so that
--   we could typeset it in upright rather than italic text, but I decided
--   to leave it out and require users to mark text with @\\textup@ instead,
--   as that allows them more flexibility about where the note appears in
--   relation to the main definition text.
newtype Definition = Definition Text
                   deriving (Eq, Show)

-- | Specifies a source for a lexicon entry.  We track location info so that
--   later, when we add a mechanism for users to specify the allowed set of
--   textbooks in the input file, we can provide useful error messages when the
--   supplied textbook isn't valid.
data Citation =
    -- | vocabulary item from a textbook
    Textbook { citePos :: Location, textbook :: Text, chapter :: Integer }
  deriving (Eq, Show)

-- | Morphological exceptions and overrides for a single entry.
type OverrideMap key = Map key Override

-- | Overriding morphology for a particular form.  Design choice: do we
--   represent 'Invalid, 'Replacement', and 'Alternative' separately, or do
--   we provide a single @Override@ form that contains a @Set Text@ and require
--   the user to include the normal form as part of the set for an alternative?
--
--   The tradeoff exchanges user convenience for simplicity of representation
--   (and of the input grammar).  I think I'll go with user convenience, at
--   least for the moment.
data Override =
    Invalid                 -- ^ Form does not exist
  | Replacement (Set Word)  -- ^ Replace regular form
  | Alternative (Set Word)  -- ^ Augment regular form
  deriving (Eq, Show)

-- | Identifies a specific form of a noun.
data NounParse = NounParse Case Number
               deriving (Eq, Ord, Show)

-- | Identifies a specific form of a verb.
data VerbParse = Finite Person Number Tense Voice Mood
               | Infinitive Tense Voice
               deriving (Eq, Ord, Show)

data Gender = Masc | Fem | Neut
            deriving (Eq, Ord, Show, Bounded, Enum)

data Case = Nom | Gen | Dat | Acc | Abl | Loc | Voc
          deriving (Eq, Ord, Show, Bounded, Enum)

data Number = Sing | Dual | Pl
            deriving (Eq, Ord, Show, Bounded, Enum)

data Person = First | Second | Third
            deriving (Eq, Ord, Show, Bounded, Enum)

data Tense = Pres | Imperf | Fut | Perf | PluPerf | FutPerf
           deriving (Eq, Ord, Show, Bounded, Enum)

data Voice = Active | Passive
           deriving (Eq, Ord, Show, Bounded, Enum)

data Mood = Indic | Imper | Subj
          deriving (Eq, Ord, Show, Bounded, Enum)

-- | General source-location information
data Location = Location { sourceName :: !String
                         , line       :: !Int
                         , col        :: !Int
                         }
                deriving (Eq, Ord, Show)

formatLocationForError :: Location -> ShowS
formatLocationForError (Location src line col) =
  (src ++) . (":" ++) . shows line . (":" ++) . shows col . (": " ++)
