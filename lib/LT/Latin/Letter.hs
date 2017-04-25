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

module LT.Latin.Letter where

data Letter = Letter { base :: Char, macron :: Macron }
            deriving (Eq, Show)

data Macron = NoMacron | Macron
  deriving (Eq, Show, Ord)

instance Ord Letter where
  compare l1 l2 =
    compareSeries [compare (base l1) (base l2),
                   compare (macron l1) (macron l2)]

compareSeries :: [Ordering] -> Ordering
compareSeries = foldl' update EQ
 where update EQ x    = x
       update accum _ = accum
