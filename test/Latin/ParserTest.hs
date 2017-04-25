{-# LANGUAGE OverloadedStrings #-}

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

module Latin.ParserTest(tests) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.HUnit

-- import LT.Text (Text)
import LT.Latin.Ast
import LT.Latin.Parser

tests = "Latin.ParserTest" ~:
  [ nounTests
  , verbTests
  , indeclTests
  , correlTests
  , generalTests
  ]

nounTests = "nounTests" ~:
  ["simple noun" ~:
   testParse (concat ["(īnsula noun f īnsulae\n",
                      "  \"island\"\n",
                      "  #:cite W 1)"])
   ~?= Right [Entry { entryPos = locn 1 1
                    , headWord = Noun "īnsula" "īnsulae"
                        (Set.singleton Fem)
                        Map.empty
                    , entryNum = Nothing
                    , note = Nothing
                    , definitions = [Definition "island"]
                    , subEntries = []
                    , citations = [Textbook (locn 3 3) "W" 1]}],

   "multiple genders" ~:
   testParse (concat ["(deus noun m f deī\n",
                      "  \"god, goddess\"\n",
                      "  #:cite W 2)"])
   ~?= Right [Entry { entryPos = locn 1 1
                    , headWord = Noun "deus" "deī"
                                   (Set.fromList [Masc, Fem])
                                   Map.empty
                    , entryNum = Nothing
                    , note = Nothing
                    , definitions = [Definition "god, goddess"]
                    , subEntries = []
                    , citations = [Textbook (locn 3 3) "W" 2]}],

   "override" ~:
   testParse (concat ["(deus noun m deī\n",
                      "  #:invalid gen pl\n",
                      "  #:augment voc sg (deī)\n",
                      "  #:replace voc pl (bogusReplacement bogusRepTwo)\n",
                      "  \"god, goddess\"\n",
                      "  #:cite W 2)\n"])
   ~?= Right [Entry { entryPos = locn 1 1
                    , headWord = Noun "deus" "deī"
                                   (Set.singleton Masc)
                                   (Map.fromList
                                    [(NounParse Voc Sing,
                                      Alternative (Set.singleton "deī")),
                                     (NounParse Gen Pl, Invalid),
                                     (NounParse Voc Pl,
                                      Replacement
                                       (Set.fromList ["bogusReplacement",
                                                      "bogusRepTwo"]))])
                    , entryNum = Nothing
                    , note = Nothing
                    , definitions = [Definition "god, goddess"]
                    , subEntries = []
                    , citations = [Textbook (locn 6 3) "W" 2]}]
   ]

verbTests = "verbTests" ~: [
  "simple verb" ~:
  testParse (concat ["(amō verb amāre amāvī amātum\n",
                     "  \"to like, love\"\n",
                     "  #:cite W 3)\n"])
  ~?= Right [Entry { entryPos = locn 1 1
                   , headWord = Verb "amō" "amāre" "amāvī" "amātum" Map.empty
                   , entryNum = Nothing
                   , note = Nothing
                   , definitions = [Definition "to like, love"]
                   , subEntries = []
                   , citations = [Textbook (locn 3 3) "W" 3]}],

  "overrides" ~:
  testParse (concat ["(dō verb dāre dēdī datum\n",
                     "  #:replace 2nd sg pres act indic (das)\n",
                     "  #:invalid pres pasv inf\n",
                     "  \"to give\"\n",
                     "  #:cite W 4)\n"])
  ~?= Right [
      Entry { entryPos = locn 1 1
            , headWord = Verb "dō" "dāre" "dēdī" "datum"
                           (Map.fromList
                             [(Finite Second Sing Pres Active Indic,
                               Replacement (Set.fromList ["das"])),
                              (Infinitive Pres Passive, Invalid)])
            , entryNum = Nothing
            , note = Nothing
            , definitions = [Definition "to give"]
            , subEntries = []
            , citations = [Textbook (locn 5 3) "W" 4]}]
  ]

indeclTests = "indeclTests" ~: [
  "basic" ~:
  testParse "(tamen \"nevertheless\" #:cite W 14)\n"
  ~?= Right [
      Entry { entryPos = locn 1 1
            , headWord = Indeclinable "tamen"
            , entryNum = Nothing
            , note = Nothing
            , definitions = [Definition "nevertheless"]
            , subEntries = []
            , citations = [Textbook (locn 1 23) "W" 14]}]
  ]

correlTests = "correlTests" ~: [
  "basic" ~:
  testParse "(et ... et \"both \\\\ldots{} and\" #:cite W 6)\n"
  ~?= Right [
      Entry { entryPos = locn 1 1
            , headWord = Correlative "et" "et"
            , entryNum = Nothing
            , note = Nothing
            , definitions = [Definition "both \\ldots{} and"]
            , subEntries = []
            , citations = [Textbook (locn 1 33) "W" 6]}]
  ]

generalTests = "generalTests" ~: [
  "multiple definitions" ~:
  testParse (concat ["(in\n",
                     "  \"(+ acc) into, onto\"\n",
                     "  \"(+ abl) in, on\"\n",
                     "  #:cite W 4)\n"])
  ~?= Right [Entry { entryPos = locn 1 1
                   , headWord = Indeclinable "in"
                   , entryNum = Nothing
                   , note = Nothing
                   , definitions = [Definition "(+ acc) into, onto",
                                    Definition "(+ abl) in, on"]
                   , subEntries = []
                   , citations = [Textbook (locn 4 3) "W" 4]}],

  "entry note" ~:
  testParse (concat ["(deus noun m deī\n",
                     "  #:note \"test note\"\n",
                     "  \"god, goddess\"\n",
                     "  #:cite W 2)"])
  ~?= Right [Entry { entryPos = locn 1 1
                   , headWord = Noun "deus" "deī"
                                  (Set.singleton Masc)
                                  Map.empty
                   , entryNum = Nothing
                   , note = Just "test note"
                   , definitions = [Definition "god, goddess"]
                   , subEntries = []
                   , citations = [Textbook (locn 4 3) "W" 2]}],

  "multiple entries" ~:
  testParse (concat ["(nauta noun m nautae\n",
                     "  \"sailor\"\n",
                     "  #:cite W 3)\n",
                     "(deus noun m deī\n",
                     "  \"god, goddess\"\n",
                     "  #:cite W 2)\n"])
  ~?= Right [Entry { entryPos = locn 1 1
                   , headWord = Noun "nauta" "nautae"
                                  (Set.singleton Masc)
                                  Map.empty
                   , entryNum = Nothing
                   , note = Nothing
                   , definitions = [Definition "sailor"]
                   , subEntries = []
                   , citations = [Textbook (locn 3 3) "W" 3]},
             Entry { entryPos = locn 4 1
                   , headWord = Noun "deus" "deī"
                                  (Set.singleton Masc)
                                  Map.empty
                   , entryNum = Nothing
                   , note = Nothing
                   , definitions = [Definition "god, goddess"]
                   , subEntries = []
                   , citations = [Textbook (locn 6 3) "W" 2]}],

  "subentry" ~:
  testParse (concat ["(poena noun f poenae\n",
                     "  \"penalty, punishment\"\n",
                     "  (\"poenās dāre\" \"to pay the penalty\"\n",
                     "   #:cite W 2)\n",
                     "  #:cite W 2)"])
  ~?= Right [Entry { entryPos = locn 1 1
                   , headWord = Noun "poena" "poenae"
                                  (Set.singleton Fem)
                                  Map.empty
                   , entryNum = Nothing
                   , note = Nothing
                   , definitions = [Definition "penalty, punishment"]
                   , subEntries = [
                       Entry { entryPos = locn 3 3
                             , headWord = Indeclinable "poenās dāre"
                             , entryNum = Nothing
                             , note = Nothing
                             , definitions = [Definition "to pay the penalty"]
                             , subEntries = []
                             , citations = [Textbook (locn 4 4) "W" 2]}]
                   , citations = [Textbook (locn 5 3) "W" 2]}]
  ]

testSrc = "test source"
testParse = parse testSrc

locn :: Int -> Int -> Location
locn = Location testSrc
