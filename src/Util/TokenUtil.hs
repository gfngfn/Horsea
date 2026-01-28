module Util.TokenUtil
  ( Span (..),
    mergeSpan,
    Located (..),
    Tokenizer,
    space,
    lowerIdent,
    upperIdent,
    longLowerIdent,
    operator,
    operatorLong,
    integerLiteral,
    floatLiteral,
    stringLiteral,
    comment,
    genLex,
  )
where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Either.Extra qualified as Either
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec qualified as Mp
import Text.Megaparsec.Char qualified as MpChar
import Text.Megaparsec.Char.Lexer qualified as MpLexer
import Prelude

-- The type for code locations (pairs of a start offset and an end offset).
data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Eq, Ord, Show)

mergeSpan :: Span -> Span -> Span
mergeSpan (Span {start}) (Span {end}) = Span {start, end}

data Located a = Located Span a
  deriving (Eq, Ord, Show, Functor)

type Tokenizer = Mp.Parsec Void Text

space :: Tokenizer ()
space = MpLexer.space MpChar.space1 empty empty

isRestChar :: Char -> Bool
isRestChar c = Char.isAlphaNum c || c == '_'

isRestCharOrDot :: Char -> Bool
isRestCharOrDot c = isRestChar c || c == '.'

lowerIdent :: Tokenizer Text
lowerIdent = Text.pack <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy Char.isLower
    p2 = Mp.many (Mp.satisfy isRestChar) <* Mp.notFollowedBy (Mp.satisfy isRestChar)

upperIdent :: Tokenizer Text
upperIdent = Text.pack <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy Char.isUpper
    p2 = Mp.many (Mp.satisfy isRestChar) <* Mp.notFollowedBy (Mp.satisfy isRestCharOrDot)

-- Parses a lowercased identifier preceded by a sequence of module names.
longLowerIdent :: Tokenizer ([Text], Text)
longLowerIdent =
  (,) <$> Mp.many (buildModuleName <$> p1 <*> (p2 <* Mp.single '.')) <*> lowerIdent
  where
    p1 = Mp.satisfy Char.isUpper
    p2 = Mp.many (Mp.satisfy isRestChar)
    buildModuleName c cs = Text.pack (c : cs)

opRestCharSet :: Set Char
opRestCharSet =
  Set.fromList ['+', '-', '*', '/', '=', '<', '>', '&']

opRestChar :: Tokenizer Char
opRestChar =
  Mp.satisfy (`elem` opRestCharSet)

operator :: Char -> Tokenizer Text
operator firstChar =
  Text.pack <$> ((:) <$> Mp.single firstChar <*> (Mp.many opRestChar <* Mp.notFollowedBy opRestChar))

operatorLong :: Char -> Tokenizer Text
operatorLong firstChar =
  Text.pack <$> ((:) <$> Mp.single firstChar <*> (Mp.some opRestChar <* Mp.notFollowedBy opRestChar))

nonzeroDigit :: Tokenizer Char
nonzeroDigit = Mp.satisfy (\c -> Char.isDigit c && c /= '0')

digit :: Tokenizer Char
digit = Mp.satisfy Char.isDigit

integerLiteralString :: Tokenizer String
integerLiteralString =
  ((:) <$> nonzeroDigit <*> Mp.many digit)
    <|> ((: []) <$> Mp.single '0')

integerLiteral :: Tokenizer Int
integerLiteral =
  (read <$> (integerLiteralString <* Mp.notFollowedBy digit))
    <|> (negate . read <$> (Mp.single '-' *> integerLiteralString <* Mp.notFollowedBy digit))

floatLiteral :: Tokenizer Double
floatLiteral =
  read <$> ((\s1 s2 -> s1 ++ "." ++ s2) <$> p1 <*> p2)
  where
    p1 = integerLiteralString <* Mp.single '.'
    p2 = Mp.some digit <* Mp.notFollowedBy digit

stringLiteral :: Tokenizer Text
stringLiteral = do
  Text.pack <$> (Mp.single '"' *> Mp.many charInStringLiteral <* Mp.single '"')

charInStringLiteral :: Tokenizer Char
charInStringLiteral =
  choice
    [ '"' <$ Mp.chunk "\\\"",
      '\\' <$ Mp.chunk "\\\\",
      '\n' <$ Mp.chunk "\\n",
      Mp.satisfy (\c -> c /= '"' && c /= '\\')
    ]

comment :: Tokenizer Text
comment =
  Text.pack . concat <$> (Mp.chunk "(*" *> Mp.many (p1 <|> p2) <* Mp.chunk "*)")
  where
    p1 = (: []) <$> Mp.satisfy (/= '*')
    p2 = Mp.try ((\c1 c2 -> [c1, c2]) <$> Mp.single '*' <*> Mp.satisfy (/= ')'))

tokenSep :: Tokenizer comment -> Tokenizer ()
tokenSep getComment = do
  () <- space
  _ <- Mp.many ((,) <$> getComment <*> space)
  pure ()

tokenWithOffsets :: Tokenizer token -> Tokenizer comment -> Tokenizer (Located token)
tokenWithOffsets getToken getComment = do
  start <- Mp.getOffset
  t <- getToken
  end <- Mp.getOffset
  () <- tokenSep getComment
  pure $ Located (Span start end) t

genLex :: Tokenizer token -> Tokenizer comment -> Text -> Either String [Located token]
genLex getToken getComment source =
  Either.mapLeft Mp.errorBundlePretty $
    Mp.parse (tokenSep getComment *> manyTill (tokenWithOffsets getToken getComment) Mp.eof) "input" source
