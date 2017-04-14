module Latin.ScannerTest(tests) where

import Test.HUnit

import LT.Latin.Ast
import LT.Latin.Scanner

tests = "Latin Scanner" ~:
  [punctuationTests,
   stringTests,
   symbolTests,
   blockCommentTests,
   numberTests]

punctuationTests =
  "punctuation" ~:
  ["open paren" ~:
   singleToken "(" ~?= Right (Tok_LParen (locn 1 1)),

   "close paren" ~:
   singleToken ")" ~?= Right (Tok_RParen (locn 1 1)),

   "ellipsis" ~:
   singleToken "..." ~?= Right (Tok_Ellipsis (locn 1 1)),

   "open paren with leading whitespace" ~:
   singleToken "  (" ~?= Right (Tok_LParen (locn 1 3)),

   "paren with line comment" ~:
   tokenList "(; test comment\n)" ~?= Right [Tok_LParen (locn 1 1),
                                             Tok_RParen (locn 2 1)]]

stringTests =
  "string literals" ~: [3 ~?= 4]

symbolTests =
  "symbols" ~: [3 ~?= 4]

blockCommentTests =
  "block comments" ~: [3 ~?= 4]

numberTests =
  "numeric literals" ~: [3 ~?= 4]

singleToken :: String -> Either String Token
singleToken = scanSingleToken testSource

tokenList :: String -> Either String [Token]
tokenList = scan testSource

testSource = "unit test input"

locn :: Int -> Int -> Location
locn l c = Location testSource l c
