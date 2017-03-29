-- -*- haskell -*-
{
module LanguageTools.Latin.Parser(parse) where

import Data.Map (Map)
import qualified Data.Map as Map

import LanguageTools.Latin.Ast
import LanguageTools.Text

}

%name parseLatin
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Tok_EOF }
%error { happyError }

%token
  '('           { Tok_LParen {} }
  ')'           { Tok_RParen {} }
  ellipsis      { Tok_Ellipsis {} }
  m             { Tok_M {} }
  f             { Tok_F {} }
  n             { Tok_N {} }
  nom           { Tok_Nom {} }
  gen           { Tok_Gen {} }
  dat           { Tok_Dat {} }
  acc           { Tok_Acc {} }
  abl           { Tok_Abl {} }
  loc           { Tok_Loc {} }
  voc           { Tok_Voc {} }
  sg            { Tok_Sg {} }
  dual          { Tok_Dual {} }
  pl            { Tok_Pl {} }
  first         { Tok_First {} }    -- first
  second        { Tok_Second {} }   -- second
  third         { Tok_Third {} }    -- third
  pres          { Tok_Pres {} }
  imperf        { Tok_Imperf {} }
  fut           { Tok_Fut {} }
  perf          { Tok_Perf {} }
  pluperf       { Tok_Pluperf {} }
  futperf       { Tok_FutPerf {} }
  act           { Tok_Act {} }
  pasv          { Tok_Pasv {} }
  indic         { Tok_Indic {} }
  imper         { Tok_Imper {} }
  subj          { Tok_Subj {} }
  inf           { Tok_Inf {} }
  kwInvalid     { Tok_kwInvalid {} }
  kwReplace     { Tok_kwReplace {} }
  kwAugment     { Tok_kwAugment {} }
  kwNote        { Tok_KwNote {} }   -- #:note
  kwCite        { Tok_KwCite {} }   -- #:cite
  string        { Tok_StrLit {} }
  symbol        { Tok_Symbol {} }
  number        { Tok_IntLit {} }

%%

InputFile :: { [Entry] }
  : NonEmptySeq(Entry)                  { $1 }

Entry :: { Entry }
  : '(' Head Body ')'                   { mkEntry (loc $1) $2 $3 }

Head :: { HeadWord }
  : Word Word
    NonEmptySeq(Gender)
    Seq(Override(NounParse))            { Noun $1 $2 (Set.fromList $3)
                                            (checkOverrides (loc $1) $4) }
  | Word Word Word Word
    Seq(Override(VerbParse))            { Verb $1 $2 $3 $4
                                            (checkOverides (loc $1) $5) }
  | Word ellipsis Word                  { Correlative $1 $3 }
  | Word                                { Indeclinable $1 }

Body :: { Body }
  : Opt(Note)
    NonEmptySeq(Definition)
    Seq(Entry)
    NonEmptySeq(Citation)               { Body $1 $2 $3 $4 }

Note :: { Text }
  : kwNote string                       { $2 }

Definition :: { Definition }
  : string                              { Definition $1 }

Citation :: { Citation }
  : kwCite symbol number                { Textbook (loc $1)
                                            (T.fromString (symval $2))
                                            (intval $3) }
Gender :: { Gender }
  : m                                   { Masc }
  | f                                   { Fem }
  | n                                   { Neut }

Case :: { Case }
  : nom                                 { Nom }
  | gen                                 { Gen }
  | dat                                 { Dat }
  | acc                                 { Acc }
  | abl                                 { Abl }
  | loc                                 { Loc }
  | voc                                 { Voc }

Number :: { Number }
  : sg                                  { Sing }
  | dual                                { Dual }
  | pl                                  { Pl }

Person :: { Person }
  : first                               { First }
  | second                              { Second }
  | third                               { Third }

Tense :: { Tense }
  : pres                                { Pres }
  | imperf                              { Imperf }
  | fut                                 { Fut }
  | perf                                { Perf }
  | pluperf                             { PluPerf }
  | futperf                             { FutPerf }

Voice :: { Voice }
  : act                                 { Active }
  | pasv                                { Passive }

Mood :: { Mood }
  : indic                               { Indic }
  | imper                               { Imper }
  | subj                                { Subj }

Word :: { Text }
Word
  : string                              { strval $1 }
  | symbol                              { symval $1 }

-- Production a -> Production (a, Override)
Override(P)
  : kwInvalid P                         { ($2, Invalid) }
  | kwReplace P
      '(' NonEmptySeq(Word) ')'         { ($2, Replacement (Set.fromList $4)) }
  | kwAugment P
      '(' NonEmptySeq(Word) ')'         { ($2, Alternative (Set.fromlist $4)) }

NounParse :: { NounParse }
  : Gender Case Number                  { NounParse $1 $2 $3 }

VerbParse :: { VerbParse }
  : Person Number Tense Voice Mood      { Finite $1 $2 $3 $4 $5 }
  | Tense Voice inf                     { Infinitive $1 $2 }

-- Production a -> Production (Maybe a)
Opt(P)
  : {- empty -}                         { Nothing }
  | P                                   { Just $1 }

-- Production a -> Production [a]; resulting list is in order but may be empty
Seq(P)
  : {- empty -}                         { [] }
  | NonEmptySeq(P)                      { $1 }

-- Production a -> Production [a]; resulting list is not empty & in order
NonEmptySeq(P)
  : RevNonEmptySeq(P)                   { reverse $1 }

-- Production a -> Production [a]; resulting list is not empty & reversed
RevNonEmptySeq(P)
  : P                                   { [$1] }
  | RevNonEmptySeq(P) P                 { $2 : $1 }

{

data Body = Body { body_note :: Maybe Text
                 , body_defns :: [Definition]
                 , body_subentries :: [Entry]
                 , body_citations :: [Citation]
                 }

mkEntry :: Location -> HeadWord -> Body -> Entry
mkEntry loc head body =
  Entry loc
        head
        Nothing
        (body_note body)
        (body_definitions body)
        (body_subentries body)
        (body_citations body)

checkOverrides :: (Ord a, Show a) => Location -> [(a, b)] -> Map a b
checkOverrides loc overrides =
  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr checkOverride Map.empty overrides
  where checkOverride (parse, override) accum
          | parse `Map.member` accum =
              alexError
                (formatLocationForError loc .
                 ("duplicate override: " ++) .
                 show parse)
          | otherwise = Map.insert parse override accum

}
