module Latin.ScannerTest(tests) where

import Test.HUnit

import LT.Text
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
  "string literals" ~:
  ["simple literal" ~:
   singleToken "\"xyz\"" ~?= Right (strtok (locn 1 1) "xyz"),

   "literal with semicolon and whitespace " ~:
   singleToken "\"a b; c d\""
   ~?= Right (strtok (locn 1 1) "a b; c d"),

   "unterminated literal" ~:
   singleToken "\"a"
   ~?= Left (mkError 1 3 "lexical error: unterminated string literal"),

   "literal with escapes" ~:
   singleToken "\"a\\\"b\\\\c\""
   ~?= Right (strtok (locn 1 1) "a\"b\\c"),

   "literal with internal newline" ~:
   singleToken "\"a\nb\""
   ~?= Left (mkError 1 3 "lexical error: newline in string literal"),

   -- Make sure the scanner doesn't get confused by the old string-literal
   -- state hanging around after the close of the first one.
   "extra quote" ~:
   tokenList "\"a\" b \" c"
   ~?= Left (mkError 1 10 "lexical error: unterminated string literal")
  ]

symbolTests =
  "symbols" ~:
  ["simple word" ~:
   singleToken "xyz" ~?= Right (symtok (locn 1 1) "xyz"),

   "word terminated by punctuation" ~:
   singleToken "xyz(abc" ~?= Right (symtok (locn 1 1) "xyz"),

   "word terminated by whitespace" ~:
   singleToken "abc def" ~?= Right (symtok (locn 1 1) "abc"),

   "attempted escape" ~:
   singleToken "abc\\(def" ~?= Right (symtok (locn 1 1) "abc")
   ]

blockCommentTests =
  "block comments" ~:
  ["simple comment" ~:
   tokenList "a #| b |# c"
   ~?= Right [symtok (locn 1 1) "a",
              symtok (locn 1 11) "c"],

   "comment with newline" ~:
   tokenList "a #| b \n c |# d"
   ~?= Right [symtok (locn 1 1) "a",
              symtok (locn 2 7) "d"],

   "comment with repeated pipes" ~:
   tokenList "a #|||||||||| b |||||||||# c"
   ~?= Right [symtok (locn 1 1) "a",
              symtok (locn 1 28) "c"],

   "nested comment" ~:
   tokenList "a #| b #| c |# d |# e"
   ~?= Right [symtok (locn 1 1) "a",
              symtok (locn 1 21) "e"],

   -- can't do top-level comment with repeated hashes, because ##### isn't a
   -- valid token.
   "nested comment with repeated hashes" ~:
   tokenList "a #| b ####| c |#### d |# e"
   ~?= Right [symtok (locn 1 1) "a",
              symtok (locn 1 27) "e"],

   "unterminated comment" ~:
   tokenList "a #| b"
   ~?= Left (mkError 1 7 "lexical error: unterminated block comment"),

   "nested unterminated comment" ~:
   tokenList "a #| b #| c"
   ~?= Left (mkError 1 12 "lexical error: unterminated block comment")
   ]

numberTests =
  "numeric literals" ~:
  ["simple numeric literal" ~:
   singleToken "234" ~?= Right (Tok_IntLit (locn 1 1) 234),

   "numeric literal with extra whitespace" ~:
   tokenList "234 567" ~?= Right [Tok_IntLit (locn 1 1) 234,
                                  Tok_IntLit (locn 1 5) 567],

   "mix of digits and letters" ~:
   singleToken "23x4" ~?= Right (symtok (locn 1 1) "23x4")]

singleToken :: String -> Either String Token
singleToken = scanSingleToken testSource

tokenList :: String -> Either String [Token]
tokenList = scan testSource

testSource = "unit test input"

locn :: Int -> Int -> Location
locn l c = Location testSource l c

strtok :: Location -> String -> Token
strtok loc s = Tok_StrLit loc (fromString s)

symtok :: Location -> String -> Token
symtok loc s = Tok_Symbol loc (fromString s)

mkError :: Int -> Int -> String -> String
mkError line col msg =
  formatLocationForError (locn line col) msg
