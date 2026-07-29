module Staged.Token
  ( Token (..),
    lex,
  )
where

import Common.TokenUtil
import Control.Monad.Combinators
import Data.Either.Extra
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Base
import GHC.Generics
import Text.Megaparsec qualified as Mp
import Prelude hiding (lex)

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftRecordParen
  | TokRightRecordParen
  | TokArrow
  | TokEqual
  | TokColon
  | TokColonColon
  | TokComma
  | TokBracket
  | TokEscape
  | TokPersistent
  | TokSemicolon
  | TokUnderscore
  | TokOpFlipApp
  | TokBar
  | TokProd
  | TokVecLeft
  | TokVecRight
  | TokMatLeft
  | TokMatRight
  | TokLower Text
  | TokUpper Text
  | TokLongLowerWithProjs ([Located Text], Located Text, [Located Text])
  | TokLongUpper ([Text], Text)
  | TokLabelNormal Text
  | TokLabelOmissible Text
  | TokTypeVar Text
  | TokInt Int
  | TokFloat Double
  | TokString Text
  | TokFun
  | TokRec
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokCase
  | TokOf
  | TokAs
  | TokVal
  | TokType
  | TokModule
  | TokStruct
  | TokEnd
  | TokExternal
  | TokOpen
  | TokTrue
  | TokFalse
  | TokForall
  | TokOpAdd Text
  | TokOpMult Text
  | TokOpComp Text
  | TokOpAnd Text
  | TokOpOr Text
  deriving stock (Ord, Eq, Show, Generic)

instance Mp.VisualStream [Located Token] where
  showTokens _proxy tokens =
    unwords (map (\(Located _ t) -> showToken t) (NonEmpty.toList tokens))

showToken :: Token -> String
showToken = \case
  TokLeftParen -> "("
  TokRightParen -> ")"
  TokLeftBrace -> "{"
  TokRightBrace -> "}"
  TokLeftSquare -> "["
  TokRightSquare -> "]"
  TokLeftRecordParen -> "(|"
  TokRightRecordParen -> "|)"
  TokArrow -> "->"
  TokEqual -> "="
  TokColon -> ":"
  TokColonColon -> "::"
  TokComma -> ","
  TokBracket -> "&"
  TokEscape -> "~"
  TokPersistent -> "%"
  TokSemicolon -> ";"
  TokUnderscore -> "_"
  TokOpFlipApp -> "|>"
  TokBar -> "|"
  TokProd -> "*"
  TokVecLeft -> "[|"
  TokVecRight -> "|]"
  TokMatLeft -> "[#"
  TokMatRight -> "#]"
  TokLower lower -> Text.unpack lower
  TokUpper upper -> Text.unpack upper
  TokLongLowerWithProjs (mods, lower, projs) ->
    Text.unpack $ Text.intercalate "." $ map ignoreSpan $ mods ++ (lower : projs)
  TokLongUpper (mods, upper) ->
    Text.unpack $ Text.intercalate "." $ mods ++ [upper]
  TokLabelNormal label -> "#" ++ Text.unpack label
  TokLabelOmissible label -> "?" ++ Text.unpack label
  TokTypeVar a -> '\'' : Text.unpack a
  TokInt n -> show n
  TokFloat r -> show r
  TokString s -> show s
  TokFun -> "fun"
  TokRec -> "rec"
  TokLet -> "let"
  TokIn -> "in"
  TokIf -> "if"
  TokThen -> "then"
  TokElse -> "else"
  TokCase -> "case"
  TokOf -> "of"
  TokAs -> "as"
  TokVal -> "val"
  TokType -> "type"
  TokModule -> "module"
  TokStruct -> "struct"
  TokEnd -> "end"
  TokExternal -> "external"
  TokOpen -> "open"
  TokTrue -> "true"
  TokFalse -> "false"
  TokForall -> "forall"
  TokOpAdd op -> Text.unpack op
  TokOpMult op -> Text.unpack op
  TokOpComp op -> Text.unpack op
  TokOpAnd op -> Text.unpack op
  TokOpOr op -> Text.unpack op

instance Mp.TraversableStream [Located Token] where
  reachOffset _n posState = (Nothing, posState) -- TODO (enhance): make this more informative

keywordMap :: Map Text Token
keywordMap =
  Map.fromList
    [ ("fun", TokFun),
      ("rec", TokRec),
      ("let", TokLet),
      ("in", TokIn),
      ("if", TokIf),
      ("then", TokThen),
      ("else", TokElse),
      ("case", TokCase),
      ("of", TokOf),
      ("as", TokAs),
      ("val", TokVal),
      ("type", TokType),
      ("module", TokModule),
      ("struct", TokStruct),
      ("end", TokEnd),
      ("external", TokExternal),
      ("open", TokOpen),
      ("true", TokTrue),
      ("false", TokFalse),
      ("forall", TokForall)
    ]

longLowerIdentWithProjsOrKeyword :: Tokenizer Token
longLowerIdentWithProjsOrKeyword = do
  t@(mods, Located _ x, projs) <- longLowerIdentWithProjs
  case (mods, projs) of
    ([], []) ->
      pure $
        case Map.lookup x keywordMap of
          Just tok -> tok
          Nothing -> TokLower x
    (_, _) ->
      -- TODO (enhance): check that `projs` do not contain keywords
      pure $ TokLongLowerWithProjs t

token :: Tokenizer Token
token =
  choice
    [ -- `(`, `)`, `{`, and `}`:
      TokLeftRecordParen <$ Mp.chunk "(|",
      TokLeftParen <$ Mp.single '(',
      TokRightRecordParen <$ Mp.chunk "|)",
      TokRightParen <$ Mp.single ')',
      TokLeftBrace <$ Mp.single '{',
      TokRightBrace <$ Mp.single '}',
      -- `[`:
      TokVecLeft <$ Mp.chunk "[|",
      TokMatLeft <$ Mp.chunk "[#",
      TokLeftSquare <$ Mp.single '[',
      -- `]`:
      TokRightSquare <$ Mp.single ']',
      -- `:`:
      TokColonColon <$ Mp.chunk "::",
      TokColon <$ Mp.single ':',
      -- `,` and `:`:
      TokComma <$ Mp.single ',',
      TokSemicolon <$ Mp.single ';',
      -- `=`:
      Mp.try (TokOpComp <$> operatorLong '='),
      TokEqual <$ Mp.single '=',
      -- `&`:
      Mp.try (TokOpAnd <$> operatorLong '&'),
      TokBracket <$ Mp.single '&',
      -- `~` and `%`:
      TokEscape <$ Mp.single '~',
      TokPersistent <$ Mp.single '%',
      -- `|`:
      TokOpFlipApp <$ Mp.chunk "|>",
      TokVecRight <$ Mp.chunk "|]",
      Mp.try (TokOpOr <$> operatorLong '|'),
      TokBar <$ Mp.single '|',
      -- `#`:
      Mp.try (TokLabelNormal <$> (Mp.single '#' *> lowerIdent)),
      TokMatRight <$ Mp.chunk "#]",
      -- `?`:
      TokLabelOmissible <$> (Mp.single '?' *> lowerIdent),
      -- `*`:
      Mp.try (TokOpMult <$> operatorLong '*'),
      TokProd <$ Mp.single '*',
      -- `+`, `/`, `<`, and `>`:
      TokOpAdd <$> operator '+',
      TokOpMult <$> operator '/',
      TokOpComp <$> operator '<',
      TokOpComp <$> operator '>',
      -- `_`:
      TokUnderscore <$ Mp.single '_',
      -- `'`:
      TokTypeVar <$> (Mp.single '\'' *> lowerIdent),
      -- identifiers:
      Mp.try longLowerIdentWithProjsOrKeyword,
      Mp.try (TokLongUpper <$> longUpperIdent),
      TokUpper <$> upperIdent,
      -- numeric literals (possibly starting with `-`):
      Mp.try (TokFloat <$> floatLiteral),
      Mp.try (TokInt <$> integerLiteral),
      -- `-`:
      TokArrow <$ Mp.chunk "->",
      TokOpAdd <$> operator '-',
      -- "`:
      TokString <$> stringLiteral
    ]

lex :: Text -> Either String [Located Token]
lex = genLex token comment
