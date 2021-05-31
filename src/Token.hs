{- |
Module      : Token
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A minimal JavaScript minifier.
-}
{-# LANGUAGE OverloadedStrings #-}
module Token where

import           NumericLiteral                 ( numericLiteral )

import           Prelude                 hiding ( takeWhile )

import           Control.Applicative            ( (<|>) )
import           Data.Attoparsec.Text
import           Data.Char                      ( isAlpha
                                                , isAlphaNum
                                                , ord
                                                )
import           Data.Functor                   ( ($>) )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


data Token = Whitespace !Text
           | EndOfLine
           | Comment !Text
           | IdentifierName !Text
           | Punctuator !Text
           | NumericLiteral !Text
           | RegExLiteral !Text
           | StringLiteral !Text
           deriving (Eq, Ord, Show)


tokenise :: Text -> Either String [Token]
tokenise = parseOnly (many' parseToken <* endOfInput)

parseToken :: Parser Token
parseToken =
  parseSingleLineComment
    <|> parseMultiLineComment
    <|> parseEol
    <|> parseWhitespace
    <|> parseNumericLiteral
    <|> parseRegExLiteral
    <|> parseStringLiteral
    <|> parsePunctuator
    <|> parseIdentifierName

parseWhitespace :: Parser Token
parseWhitespace = Whitespace <$> takeWhile1 isEcmaSpace


parseSingleLineComment :: Parser Token
parseSingleLineComment = Comment . ("//" <>) <$> ("//" *> takeTill isEcmaLineTerminator)

-- | Used only in 'parseMultiLineComment' to avoid boolean blindness.
data PrevCharInComment = Star | NotStar deriving (Eq, Ord, Show)

parseMultiLineComment :: Parser Token
parseMultiLineComment = Comment . ("/*" <>) . (<> "/") <$> (("/*" *> scan NotStar go) <* char '/')
 where
  go :: PrevCharInComment -> Char -> Maybe PrevCharInComment
  go Star '/' = Nothing
  go _    '*' = Just Star
  go _    _   = Just NotStar


-- | Returns 'True' for any character that ECMAscript considers to be white space.
isEcmaSpace :: Char -> Bool
isEcmaSpace c = ord c `IntSet.member` ecmaWhiteSpaceCPs

-- | Whitespace characters per the [ECMAscript standard](https://262.ecma-international.org/11.0/#sec-white-space).
ecmaWhiteSpaceCPs :: IntSet
ecmaWhiteSpaceCPs = IntSet.fromList
  [ 0x0009
  , 0x000b
  , 0x000c
  , 0x0020
  , 0x00a0
  , 0x1680
  , 0x2000
  , 0x2001
  , 0x2002
  , 0x2003
  , 0x2004
  , 0x2005
  , 0x2006
  , 0x2007
  , 0x2008
  , 0x2009
  , 0x200a
  , 0x202f
  , 0x205f
  , 0x3000
  , 0xfeff
  ]

parseEol :: Parser Token
parseEol = ecmaEndOfLine $> EndOfLine

-- | ECMAscript-flavoured 'endOfLine'.
ecmaEndOfLine :: Parser ()
ecmaEndOfLine = crlf <|> singleEol
 where
  crlf      = "\r\n" $> ()
  singleEol = satisfy isEcmaLineTerminator $> ()

-- | Returns 'True' for any character that ECMAscript considers to be a line terminator.
isEcmaLineTerminator :: Char -> Bool
isEcmaLineTerminator c = ord c `IntSet.member` ecmaLineTerminator

-- | Line terminator code points per the [ECMAscript standard](https://262.ecma-international.org/11.0/#sec-line-terminators).
ecmaLineTerminator :: IntSet
ecmaLineTerminator = IntSet.fromList [0x000a, 0x000d, 0x2028, 0x2029]

-- | Punctuators per the [ECMAscript standard](https://262.ecma-international.org/11.0/#sec-punctuators).
parsePunctuator :: Parser Token
parsePunctuator = Punctuator <$> ps
 where
  ps =
    "{"
      <|> "("
      <|> ")"
      <|> "["
      <|> "]"
      <|> "."
      <|> "..."
      <|> ";"
      <|> ","
      <|> "<"
      <|> ">"
      <|> "<="
      <|> ">="
      <|> "=="
      <|> "!="
      <|> "==="
      <|> "!=="
      <|> "+"
      <|> "-"
      <|> "*"
      <|> "%"
      <|> "**"
      <|> "++"
      <|> "--"
      <|> "<<"
      <|> ">>"
      <|> ">>>"
      <|> "&"
      <|> "|"
      <|> "^"
      <|> "!"
      <|> "~"
      <|> "&&"
      <|> "||"
      <|> "??"
      <|> "?"
      <|> ":"
      <|> "="
      <|> "+="
      <|> "-="
      <|> "*="
      <|> "%="
      <|> "**="
      <|> "<<="
      <|> ">>="
      <|> ">>>="
      <|> "&="
      <|> "|="
      <|> "^="
      <|> "=>"
      <|> "/"
      <|> "/="
      <|> "}"

-- | /IdentifierName/ tokens __not strictly__ per the [ECMAscript standard](https://262.ecma-international.org/11.0/#sec-names-and-keywords).
-- In particular, this fails to handle any tricky Unicode.
parseIdentifierName :: Parser Token
parseIdentifierName = IdentifierName <$> takeWhile1 (\c -> isAlphaNum c || c == '$' || c == '_')


-- | Used only in 'parseDelEscLit' to avoid boolean blindness.
data PrevCharInLiteral = Backslash | NotBackslash deriving (Eq, Ord, Show)

-- | Parse a string literal, delimited by a single character, that may contain
-- characters escaped by a backslash.  The delimiters are included in the
-- result.
parseDelEscLit :: Char -> Parser Text
parseDelEscLit delim =
  T.cons delim . flip T.snoc delim <$> (char delim *> scan NotBackslash go) <* char delim
 where
  go :: PrevCharInLiteral -> Char -> Maybe PrevCharInLiteral
  go Backslash _ = Just NotBackslash
  go NotBackslash c | c == delim = Nothing
                    | c == '\\'  = Just Backslash
                    | otherwise  = Just NotBackslash


parseStringLiteral :: Parser Token
parseStringLiteral = StringLiteral <$> (parseDelEscLit '\'' <|> parseDelEscLit '"')

parseRegExLiteral :: Parser Token
parseRegExLiteral = (\r f -> RegExLiteral (r <> f)) <$> rePart <*> flagPart
 where
  rePart   = parseDelEscLit '/'
  flagPart = takeWhile isAlpha


parseNumericLiteral :: Parser Token
parseNumericLiteral = NumericLiteral <$> numericLiteral
