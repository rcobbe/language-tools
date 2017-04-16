{-# LANGUAGE OverloadedStrings #-}

module Latin.ParserTest(tests) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.HUnit

-- import LT.Text (Text)
import LT.Latin.Ast
import LT.Latin.Parser

tests = "Latin.ParserTest" ~:
  [ nounTests
  -- , verbTests
  -- , indeclTests
  -- , correlTests
  -- , subentryTests
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

   "override" ~:
   testParse (concat ["(deus noun m deī\n",
                      "  #:invalid gen pl\n",
                      "  #:augment voc sg (deī)\n",
                      "  #:replace voc pl (bogusReplacement)\n",
                      "  \"god, goddess\"\n"
                      "  #:cite W 2)\n"])
   ~?= Right [Entry { entryPos = locn 1 1
                    , headWord = Noun "deus" "deī"
                                   (Set.singleton Masc)
                                   (Map.singleton (NounParse Masc Voc Sg)
                                      (Alternative (Set.singleton "deī")))
                    , entryNum = Nothing
                    , note = Nothing
                    , definitions = [Definition "god, goddess"]
                    , subentries = []
                    , citations = [Textbook (locn 4 3) "W" 2]}]
   ]

testSrc = "test source"
testParse = parse testSrc

locn :: Int -> Int -> Location
locn = Location testSrc
