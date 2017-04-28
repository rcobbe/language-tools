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

import Control.Monad.Trans.Except (Except)
import qualified Data.Char as Char
import Data.Function (on)

-- | Represents a single Latin letter, with macron.  We represent macrons only
--   for output representation and not for vowel length, as length is not
--   otherwise significant (unlike Greek).
data Letter = Letter { base :: Char, macron :: Macron }
            deriving (Eq, Show)

-- | Represents a Latin word
newtype Word = Word { letters :: [Letter] }
  deriving (Eq)

data Macron = NoMacron | Macron
  deriving (Eq, Show, Ord)

instance Ord Letter where
  -- | Compare two Latin letters.  The base letter is most significant,
  --   followed by macrons, followed by case.
  compare l1 l2 =
    compareSeries [compare (Char.toLower (base l1)) (Char.toLower (base l2)),
                   compare (macron l1) (macron l2),
                   compare (base l1) (base l2)]

instance Ord Word where
  -- | Compares two Latin words.  The base letters are most significant,
  --   followed by macrons, followed by case.
  compare (Word w1) (Word w2) =
    compareSeries
      [listCompare (compare `on` (Char.toLower . base)) w1 w2,
       listCompare (compare `on` macron) w1 w2,
       listCompare (compare `on` base) w1 w2]

instance Show Letter where
  show (Letter b NoMacron) = [b]
  show (Letter b Macron) = "long " ++ [b]

instance Show Word where
  show (Word letters) =
    "Word [" ++ (Map.intercalate ", " (map show letters)) ++ "]"

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
compareSeries = foldl' update EQ
 where update EQ x    = x
       update accum _ = accum

-- | Constructs a letter
makeLetter :: Char -> Macron -> Letter
makeLetter base macron =
  if validLetter base macron
  then Letter base macron
  else error (intercalate " " ["makeLetter: invalid combination:",
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
                | InvalidMacro { offset :: !Int }
                  -- ^ Macron before invalid letter (consonant or diphthong)
  deriving (Eq, Show)

-- | Parse a single word, which must extend to the end of the input
parseWord :: String -> Except ParseError Word
parseWord src =
  do CM.when (null src) (throwE EmptyInput)
     letters <- wordLoop 0 src
     return $ Word letters

-- | Main loop for parsing a word; the first argument is the offset of the
--   current letter (equivalently, the number of letters previoulsy parsed).
wordLoop :: Int -> String -> Except ParseError [Letter]
wordLoop _     []  = return []
wordLoop index src =
  do (letter, rest) <- parseLetter index src
     restResult <- wordLoop (index + 1) rest
     return $ letter : restResult

-- | Parse a single letter; the first argument is the letter offset of the
--   input.
parseLetter :: Int -> String -> Except ParseError (Letter, String)
parseLetter index [] =
  throwE $ InternalError index "parseLetter: empty input"
parseLetter index src =
  do let (macrons, rest) = span isSrcMacron src
     CM.when (null rest) (throwE $ MissingLetter index)
      (base, hasCombiningMacron) =

-- | Recognize the ways in which a macron can be expressed in the input.
--   Allow underscores, macron characters, and combining macrons.
isSrcMacron c =
  c == '_' || c == '\x00AF' || c == '\x0304'
