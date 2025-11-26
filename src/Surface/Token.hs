module Surface.Token
  ( Token (..),
    lex,
  )
where

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
import Util.TokenUtil
import Prelude hiding (lex)

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokArrow
  | TokEqual
  | TokColon
  | TokComma
  | TokSemicolon
  | TokUnderscore
  | TokOpFlipApp
  | TokProd
  | TokVecLeft
  | TokVecRight
  | TokMatLeft
  | TokMatRight
  | TokLower Text
  | TokUpper Text
  | TokLongLower ([Text], Text)
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
  | TokAs
  | TokOpen
  | TokTrue
  | TokFalse
  | TokOpAdd Text
  | TokOpMult Text
  | TokOpComp Text
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
  TokArrow -> "->"
  TokEqual -> "="
  TokColon -> ":"
  TokComma -> ","
  TokSemicolon -> ";"
  TokUnderscore -> "_"
  TokOpFlipApp -> "|>"
  TokProd -> "*"
  TokVecLeft -> "[|"
  TokVecRight -> "|]"
  TokMatLeft -> "[#"
  TokMatRight -> "#]"
  TokLower lower -> Text.unpack lower
  TokUpper upper -> Text.unpack upper
  TokLongLower (mods, lower) -> Text.unpack (Text.intercalate "." mods <> lower)
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
  TokAs -> "as"
  TokOpen -> "open"
  TokTrue -> "true"
  TokFalse -> "false"
  TokOpAdd op -> Text.unpack op
  TokOpMult op -> Text.unpack op
  TokOpComp op -> Text.unpack op

instance Mp.TraversableStream [Located Token] where
  reachOffset _n posState = (Nothing, posState)

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
      ("as", TokAs),
      ("open", TokOpen),
      ("true", TokTrue),
      ("false", TokFalse)
    ]

lowerIdentOrKeyword :: Tokenizer Token
lowerIdentOrKeyword = do
  t <- lowerIdent
  pure $ case Map.lookup t keywordMap of
    Just tok -> tok
    Nothing -> TokLower t

token :: Tokenizer Token
token =
  choice
    [ TokLeftParen <$ Mp.single '(',
      TokRightParen <$ Mp.single ')',
      TokLeftBrace <$ Mp.single '{',
      TokRightBrace <$ Mp.single '}',
      TokArrow <$ Mp.chunk "->",
      TokColon <$ Mp.single ':',
      TokComma <$ Mp.single ',',
      Mp.try (TokOpComp <$> operatorLong '='),
      TokEqual <$ Mp.single '=',
      TokSemicolon <$ Mp.single ';',
      TokUnderscore <$ Mp.single '_',
      TokOpFlipApp <$ Mp.chunk "|>",
      TokVecLeft <$ Mp.chunk "[|",
      TokVecRight <$ Mp.chunk "|]",
      TokMatLeft <$ Mp.chunk "[#",
      TokMatRight <$ Mp.chunk "#]",
      TokLeftSquare <$ Mp.single '[',
      TokRightSquare <$ Mp.single ']',
      TokOpAdd <$> operator '+',
      TokOpAdd <$> operator '-',
      Mp.try (TokOpMult <$> operatorLong '*'),
      TokProd <$ Mp.single '*',
      TokOpMult <$> operator '/',
      TokOpComp <$> operator '<',
      TokOpComp <$> operator '>',
      lowerIdentOrKeyword,
      Mp.try (TokLongLower <$> longLowerIdent),
      TokUpper <$> upperIdent,
      Mp.try (TokFloat <$> floatLiteral),
      TokInt <$> integerLiteral,
      TokString <$> stringLiteral
    ]

lex :: Text -> Either String [Located Token]
lex = genLex token comment
