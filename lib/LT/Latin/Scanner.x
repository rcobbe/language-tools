{ -- -*- haskell -*-
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module LT.Latin.Scanner(
  Token(..),
  Alex,
  runAlex,
  alexMonadScan,
  alexError,
  initSourceName,
  scan,
  scanSingleToken)
where

import qualified Control.Monad as CM
import qualified Data.Char as DC

import LT.Latin.Ast
import LT.Text (Text)
import qualified LT.Text as T

}

%wrapper "monadUserState"

$symbolLetter = $printable # \x22 # \\ # \# # $white # \( # \)

@lineComment = \; .* \n

tokens :-

<0>       $white+               ;
<0>       @lineComment          ;

<0>       "("                   { keyword Tok_LParen }
<0>       ")"                   { keyword Tok_RParen }
<0>       "..."                 { keyword Tok_Ellipsis }
<0>       "noun"                { keyword Tok_Noun }
<0>       "verb"                { keyword Tok_Verb }
<0>       "m"                   { keyword Tok_M }
<0>       "f"                   { keyword Tok_F }
<0>       "n"                   { keyword Tok_N }
<0>       "nom"                 { keyword Tok_Nom }
<0>       "gen"                 { keyword Tok_Gen }
<0>       "dat"                 { keyword Tok_Dat }
<0>       "acc"                 { keyword Tok_Acc }
<0>       "abl"                 { keyword Tok_Abl }
<0>       "loc"                 { keyword Tok_Loc }
<0>       "voc"                 { keyword Tok_Voc }
<0>       "sg"                  { keyword Tok_Sg }
<0>       "dual"                { keyword Tok_Dual }
<0>       "pl"                  { keyword Tok_Pl }
<0>       "1st"                 { keyword Tok_First }
<0>       "2nd"                 { keyword Tok_Second }
<0>       "3rd"                 { keyword Tok_Third }
<0>       "pres"                { keyword Tok_Pres }
<0>       "imperf"              { keyword Tok_Imperf }
<0>       "fut"                 { keyword Tok_Fut }
<0>       "perf"                { keyword Tok_Perf }
<0>       "pluperf"             { keyword Tok_Pluperf }
<0>       "futperf"             { keyword Tok_FutPerf }
<0>       "act"                 { keyword Tok_Act }
<0>       "pasv"                { keyword Tok_Pasv }
<0>       "indic"               { keyword Tok_Indic }
<0>       "imper"               { keyword Tok_Imper }
<0>       "subj"                { keyword Tok_Subj }
<0>       "inf"                 { keyword Tok_Inf }
<0>       "#:invalid"           { keyword Tok_KwInvalid }
<0>       "#:replace"           { keyword Tok_KwReplace }
<0>       "#:augment"           { keyword Tok_KwAugment }
<0>       "#:note"              { keyword Tok_KwNote }
<0>       "#:cite"              { keyword Tok_KwCite }
<0>       "|#"                  { commentCloseError }

<0>       $symbolLetter+        { symbolOrIntLit }


<0>       \x22                  { initString `andBegin` string }
<string>  \x22                  { finishString `andBegin` 0 }
<string>  \\ \\                 { addStringEscape '\\' }
<string>  \\ \x22               { addStringEscape '"' }
<string>  \\ .                  { invalidStringEscape }
<string>  [^\\\x22]+            { addStringChars }
<string>  \n                    { stringNewline }

<0>       "#|"                  { enterBlockComment `andBegin` blkcomm }
<blkcomm> "#"+"|"               { enterBlockComment }
<blkcomm> "|"+"#"               { leaveBlockComment }
<blkcomm> [^\#\|]+              ;
<blkcomm> \n+                   ;
<blkcomm> "#"+[^\#\|]*          ;
<blkcomm> "|"+[^\|\#]*          ;

{

data Token =
    Tok_LParen    { loc :: Location }
  | Tok_RParen    { loc :: Location }
  | Tok_Ellipsis  { loc :: Location }
  | Tok_Noun      { loc :: Location }
  | Tok_Verb      { loc :: Location }
  | Tok_M         { loc :: Location }
  | Tok_F         { loc :: Location }
  | Tok_N         { loc :: Location }
  | Tok_Nom       { loc :: Location }
  | Tok_Gen       { loc :: Location }
  | Tok_Dat       { loc :: Location }
  | Tok_Acc       { loc :: Location }
  | Tok_Abl       { loc :: Location }
  | Tok_Loc       { loc :: Location }
  | Tok_Voc       { loc :: Location }
  | Tok_Sg        { loc :: Location }
  | Tok_Dual      { loc :: Location }
  | Tok_Pl        { loc :: Location }
  | Tok_First     { loc :: Location }
  | Tok_Second    { loc :: Location }
  | Tok_Third     { loc :: Location }
  | Tok_Pres      { loc :: Location }
  | Tok_Imperf    { loc :: Location }
  | Tok_Fut       { loc :: Location }
  | Tok_Perf      { loc :: Location }
  | Tok_Pluperf   { loc :: Location }
  | Tok_FutPerf   { loc :: Location }
  | Tok_Act       { loc :: Location }
  | Tok_Pasv      { loc :: Location }
  | Tok_Indic     { loc :: Location }
  | Tok_Imper     { loc :: Location }
  | Tok_Subj      { loc :: Location }
  | Tok_Inf       { loc :: Location }
  | Tok_KwInvalid { loc :: Location }
  | Tok_KwReplace { loc :: Location }
  | Tok_KwAugment { loc :: Location }
  | Tok_KwNote    { loc :: Location }
  | Tok_KwCite    { loc :: Location }
  | Tok_StrLit    { loc :: Location, strval :: Text }
  | Tok_Symbol    { loc :: Location, symval :: Text }
  | Tok_IntLit    { loc :: Location, intval :: Integer }
  | Tok_EOF
  deriving (Eq, Show)

data AlexUserState =
  AlexUserState { aus_sourceName :: String
                  -- ^ Source name for error locations
                , stringStart :: Maybe AlexPosn
                  -- ^ If in string, posn of open quote
                , stringChars :: [Char]
                  -- ^ If in string, chars seen so far, reversed
                , blockCommentStack :: [AlexPosn]
                  -- ^ start location of all open block comments, with the
                  --   most recently opened location at the front of the list
                }

alexInitUserState = AlexUserState { aus_sourceName = ""
                                  , stringStart = Nothing
                                  , stringChars = []
                                  , blockCommentStack = [] }

-- | Initializes source information in user state (necessary because we can't
--   parameterize alexInitUserState)
initSourceName :: String -> Alex ()
initSourceName src =
  updateUserState (\s -> s { aus_sourceName = src })

-- | Function wrapper for scanMonad that returns all tokens.  I believe this
--   scans the entire input before returning a single token, so this is best
--   used only with small input, as in unit tests.  (The main Happy parser just
--   invokes alexMonadScan directly.)
scan :: String -> String -> Either String [Token]
scan src input = runAlex input (scanMonad src)

-- | 'Alex' computation that scans until reaching EOF or a scanning error
scanMonad :: String -> Alex [Token]
scanMonad src = (initSourceName src) >> tokenLoop
  where tokenLoop =
          do t <- alexMonadScan
             case t of
               Tok_EOF -> return []
               _       -> tokenLoop >>= return . (t :)

-- | Read a single token out of the input
scanSingleToken :: String -> String -> Either String Token
scanSingleToken src input =
  runAlex input ((initSourceName src) >> alexMonadScan)

-- | Action to take on reaching end of input
alexEOF :: Alex Token
alexEOF =
  do c <- alexGetStartCode
     AlexState { alex_pos = pos } <- getAlexState
     if | c == 0 -> return Tok_EOF
        | c == string ->
            lexicalError pos "unterminated string literal"
        | c == blkcomm ->
            lexicalError pos "unterminated block comment"
        | otherwise ->
            internalScannerError "alexEOF" pos ("unrecognized state " ++ show c)

-- | Wrapper for a keyword token ctor that needs only position info
keyword :: (Location -> Token) -> AlexAction Token
keyword tokenCtor (posn, _, _, _) _ =
  makeLocation posn >>= (return . tokenCtor)

-- | Action to take on scanning a symbol literal (which might actually be
--   an int lit).
symbolOrIntLit :: AlexAction Token
symbolOrIntLit (posn, _, _, s) len =
  do locn <- makeLocation posn
     let lexeme = take len s
     if all DC.isDigit lexeme
       then return $ Tok_IntLit locn (read lexeme)
       else return $ Tok_Symbol locn (T.fromString lexeme)

-- | Action to take upon scanning the double quote at the beginning of a string
--   literal
initString :: AlexAction Token
initString (posn, _, _, _) _ =
  do updateUserState (\s -> s { stringStart = Just posn, stringChars = [] })
     alexMonadScan

addStringChars :: AlexAction Token
addStringChars (posn, _, _, []) _ =
  internalScannerError "addStringChars" posn "empty input"
addStringChars (posn, _, _, _) 0 =
  internalScannerError "addStringChars" posn "zero length"
addStringChars (_, _, _, input) len =
  do updateUserState (addStringCharsToState (take len input))
     alexMonadScan

addStringEscape :: Char -> AlexAction Token
addStringEscape c _ _ =
  do updateUserState (addStringCharsToState [c])
     alexMonadScan

addStringCharsToState :: [Char] -> AlexUserState -> AlexUserState
addStringCharsToState cs ust =
  ust { stringChars = (reverse cs) ++ (stringChars ust) }

stringNewline :: AlexAction a
stringNewline (posn, _, _, _) _ =
  lexicalError posn "newline in string literal"

-- | Action to take upon scanning the double quote at the end of a string
--   literal.
finishString :: AlexAction Token
finishString (posn, _, _, _) _ =
  do st <- getUserState
     case stringStart st of
       Nothing ->
         internalScannerError "finish string" posn "no start position"
       Just startPos ->
         do startLocn <- makeLocation startPos
            -- resetting the state isn't strictly necessary, but it makes it
            -- easier to interpret an AlexUserState value
            updateUserState
              (\s -> s { stringStart = Nothing, stringChars = [] })
            return $
              Tok_StrLit startLocn (T.fromString (reverse (stringChars st)))

-- | Action that signals an error upon scanning an unsupported string escape
--   sequence.
invalidStringEscape :: AlexAction a
invalidStringEscape (posn, _, _, s) len =
  lexicalError posn ("invalid string escape: " ++ (show (take len s)))

enterBlockComment :: AlexAction Token
enterBlockComment (posn, _, _, _) _ =
  do updateUserState
       (\ust -> ust { blockCommentStack = posn : (blockCommentStack ust) })
     alexMonadScan

leaveBlockComment :: AlexAction Token
leaveBlockComment (posn, _, _, _) _ =
  do ust@(AlexUserState { blockCommentStack = commentStack }) <- getUserState
     CM.when (null commentStack)
       (internalScannerError "leaveBlockComment"
          posn "blockCommentStack is empty")
     let newCommentStack = tail commentStack
     putUserState $ ust { blockCommentStack = newCommentStack }
     CM.when (null newCommentStack) (alexSetStartCode 0)
     alexMonadScan

commentCloseError :: AlexAction a
commentCloseError (posn, _, _, _) _ =
  lexicalError posn "unexpected |#"

getUserState :: Alex AlexUserState
getUserState = Alex (\s -> Right (s, alex_ust s))

putUserState :: AlexUserState -> Alex ()
putUserState newState =
  Alex (\s -> Right (s { alex_ust = newState }, ()))

updateUserState :: (AlexUserState -> AlexUserState) -> Alex ()
updateUserState update =
  getUserState >>= (putUserState . update)

internalScannerError :: String -> AlexPosn -> String -> Alex a
internalScannerError fnName posn msg =
  do loc <- makeLocation posn
     alexError (formatLocationForError loc
                 (concat ["internal scanner error (",
                          fnName,
                          "): ",
                          msg]))

lexicalError :: AlexPosn -> String -> Alex a
lexicalError posn msg =
  do loc <- makeLocation posn
     alexError (formatLocationForError loc ("lexical error: " ++ msg))

getAlexState :: Alex AlexState
getAlexState = Alex (\s -> Right (s, s))

makeLocation :: AlexPosn -> Alex Location
makeLocation (AlexPn _ l c) =
  do (AlexUserState { aus_sourceName = srcName }) <- getUserState
     return $ Location { sourceName = srcName, line = l, col = c }

}
