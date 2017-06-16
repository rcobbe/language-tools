module Latin.LetterGen(
    LikelyEqualWords(..)
  , PrefixWords(..)
  , LengthWords(..)
  , CaseWords(..)
  )
where

import LT.Latin.Letter

import Test.QuicCheck

-- | Concrete representation of 'Letter', for use by generators

-- | Contains two 'Word' values; when created by the generator, there is a
--   50% chance that the two words are equal
data LikelyEqualWords Word Word
  deriving Show

instance Arbitrary Word where
-- | Generate an arbitrary 'Word' of 'Letter' values.  Each individual letter
--   is guaranteed to be valid, but word-level invariants (e.g., vowel before
--   "ns" is long) are not guaranteed.
