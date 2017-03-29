module LanguageTools.Latin.Ast where

import Data.Map (Map)
import Data.Set (Set)

import LanguageTools.Text (Text)

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
data HeadWord = Noun { nom :: Text
                     , gen :: Text
                     , genders :: Set Gender
                       -- ^ must be non-empty
                     , n_overrides :: OverrideMap NounParse
                     }
              | Verb { pp1 :: Text
                     , pp2 :: Text
                     , pp3 :: Text
                     , pp4 :: Text
                     , v_overrides :: OverrideMap VerbParse
                     }
              | Correlative Text Text
              | Indeclinable Text

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

-- | Overriding morphology for a particular form
data Override =
    Invalid                 -- ^ Form does not exist
  | Replacement (Set Text)  -- ^ Replace regular form
  | Alternative (Set Text)  -- ^ Augment regular form

-- | Identifies a specific form of a noun.
data NounParse = NounParse Gender Case Number
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
  (src ++) . shows ':' . shows line . shows ':' . shows col . (": " ++)
