{-# LANGUAGE ScopedTypeVariables #-}

module Latin.LetterGen(
    LikelyEqualWords(..)
  , PrefixWords(..)
  , LengthWords(..)
  , CaseWords(..)
  )
where

import Prelude hiding (Word)
import qualified Data.Char as Char
import qualified Data.Set as Set
import System.Random (Random(..))
import qualified System.Random as Random

import LT.Latin.Letter

import Test.QuickCheck

-- | Concrete representation of 'Letter', for use by generators
data LetterProps = LetterProps { propBase :: Char, propMacron :: Macron }
  deriving (Eq, Show)

-- | Contains two 'Word' values; when created by the generator, there is a
--   50% chance that the two words are equal
data LikelyEqualWords = LikelyEqualWords Word Word
  deriving Show

instance Arbitrary LikelyEqualWords where
  arbitrary =
    do (w1, w2) <- arbitrary
       equal <- choose (False, True)
       return $ LikelyEqualWords w1 (if equal then w1 else w2)

-- | Contains two 'Word' values; when created by the generator, the first word
--   is a prefix of the second
data PrefixWords = PrefixWords Word Word
  deriving Show

instance Arbitrary PrefixWords where
  arbitrary =
    do w <- arbitrary
       let ls = letters w
       prefixSize <- choose (1, length ls)
       return $ PrefixWords (makeWord (take prefixSize ls)) w

-- | Contains two 'Word' values.  When created by the generator, the two words
--   differ only in vowel length and letter case.
data LengthWords = LengthWords Word Word
  deriving Show

instance Arbitrary LengthWords where
  arbitrary =
    do (CaseWords w1 w2) <- arbitrary
       w2' <- updateWord w2 updateMacron [NoMacron, Macron]
       return $ LengthWords w1 w2'

-- | Contains two 'Word' values.  When created by the generator, the two
--   words differ only in letter case.
data CaseWords = CaseWords Word Word
  deriving (Show)

instance Arbitrary CaseWords where
  arbitrary =
    do w1 <- arbitrary
       w2 <- updateWord w1 updateCase [Char.toLower, Char.toUpper]
       return $ CaseWords w1 w2
  shrink (CaseWords w1 w2) =
    case (letters w1, letters w2) of
      (x1:x2:xs, y1:y2:ys) -> [CaseWords (makeWord (x2:xs)) (makeWord (y2:ys))]
      _ -> []

instance Arbitrary Word where
-- | Generate an arbitrary 'Word' of 'Letter' values.  Each individual letter
--   is guaranteed to be valid, but word-level invariants (e.g., vowel before
--   "ns" is long) are not guaranteed.
  arbitrary =
    do size <- choose ((1, 30) :: (Int, Int))
       (letters :: [Letter]) <- vectorOf size arbitrary
       return $ makeWord letters

instance Arbitrary Letter where
  -- | Generate an arbitrary but valid 'Letter' value.
  arbitrary = (arbitrary `suchThat` validProps) >>= (return . propsToLetter)

instance Arbitrary LetterProps where
  -- | Generate an arbitrary but valid combination of letter properties.
  arbitrary =
    do base <- elements (Set.elems baseChars)
       macron <- elements [NoMacron, Macron]
       return $ LetterProps base macron

validProps :: LetterProps -> Bool
validProps (LetterProps base macron) = validLetter base macron

updateWord :: Word -> (LetterProps -> a -> LetterProps) -> [a] -> Gen Word
updateWord baseWord updateProp values =
  (mapM (updateLetter updateProp values) (letters baseWord)) >>=
    (return . makeWord)

-- | @updateLetter updateProp values baseLetter@ is a generator that produces
--   @updateProp baseLetter val@ (with appropriate conversions between 'Letter'
--   and 'LetterProps'), where @val@ is a value selected from @values@ at
--   random, such that @val@ is a valid letter.
updateLetter :: (LetterProps -> a -> LetterProps) -> [a] -> Letter -> Gen Letter
updateLetter updateProp values baseLetter =
  let baseProps = letterToProps baseLetter
      newPropIsValid = validProps . (updateProp baseProps)
  in do newProp <- elements values `suchThat` newPropIsValid
        return (propsToLetter (updateProp baseProps newProp))

updateMacron :: LetterProps -> Macron -> LetterProps
updateMacron props newMacron = props { propMacron = newMacron }

updateCase :: LetterProps -> (Char -> Char) -> LetterProps
updateCase props f = props { propBase = f (propBase props) }

letterToProps :: Letter -> LetterProps
letterToProps l = LetterProps (base l) (macron l)

propsToLetter :: LetterProps -> Letter
propsToLetter (LetterProps b m) = makeLetter b m
