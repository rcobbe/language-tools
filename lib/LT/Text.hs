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

-- | Defines an abstract type to represent Unicode text while allowing us to
--   switch underlying representations.  The precise set of operations defined
--   is somewhat ad-hoc.

module LT.Text(
    Text
  , fromString
  , toString
  , singleton
  , cons
  , LT.Text.null
  , LT.Text.init
  , LT.Text.last
  , append
  , LT.Text.concat
  , LT.Text.concatMap
  , intercalate
  , LT.Text.reverse
  ) where

import qualified Data.String as String
import qualified Data.Text as T

newtype Text = Text { unbox :: T.Text }
             deriving (Eq, Ord, Show)

instance String.IsString Text where
  fromString = fromString

-- | Create a 'Text' from a 'String'
fromString :: String -> Text
fromString = Text . T.pack

-- | Convert a 'Text' to a 'String'
toString :: Text -> String
toString = T.unpack . unbox

-- | Create a 'Text' containing a single character
singleton :: Char -> Text
singleton = Text . T.singleton

-- | Prepend a single character to the front of a 'Text'
cons :: Char -> Text -> Text
cons c = Text . (T.cons c) . unbox

-- | Does the 'Text' represent the empty string?
null :: Text -> Bool
null  = T.null . unbox

-- | Return a 'Text' containing all but the last character of the argument.
--   The argument must not be the empty string.
init :: Text -> Text
init = Text . T.init . unbox

-- | Return the last character of a 'Text', which must not be empty.
last :: Text -> Char
last = T.last . unbox

-- | Append two 'Text' values
append :: Text -> Text -> Text
append t1 t2 = Text (T.append (unbox t1) (unbox t2))

-- | Concatenate a sequence of 'Text' values
concat :: [Text] -> Text
concat = Text . T.concat . map unbox

-- | Apply a function to each character of a 'Text' nand concatenate the
--   results.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f = Text . T.concatMap (unbox . f) . unbox

-- | @intercalate sep texts@ intersperses @sep@ between elements in @texts@ and
--   concatenates the result.
intercalate :: Text -> [Text] -> Text
intercalate sep = Text . T.intercalate (unbox sep) . map unbox

-- | Reverse a 'Text' value.
reverse :: Text -> Text
reverse = Text . T.reverse . unbox
