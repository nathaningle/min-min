{- |
Module      : NumericLiteral
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Recognise the text of an [ECMAscript numeric literal](https://262.ecma-international.org/11.0/#sec-literals-numeric-literals).
-}
{-# LANGUAGE OverloadedStrings #-}
module NumericLiteral where

import           Prelude                 hiding ( takeWhile )

import           Control.Applicative            ( (<|>) )
import           Data.Attoparsec.Text
import           Data.Char                      ( isDigit
                                                , isHexDigit
                                                , isOctDigit
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


andThen :: Semigroup a => Parser a -> Parser a -> Parser a
andThen p q = (<>) <$> p <*> q


numericLiteral, decimalBigIntegerLiteral, nonDecimalIntegerLiteral, decimalLiteral, decimalIntegerLiteral, nonZeroDigit, exponentPart, exponentIndicator, signedInteger, binaryIntegerLiteral, octalIntegerLiteral, hexIntegerLiteral
  :: Parser Text
numericLiteral =
  decimalLiteral <|> decimalBigIntegerLiteral <|> (nonDecimalIntegerLiteral `andThen` option "" "n")

decimalBigIntegerLiteral = "0n" <|> nonZeroDigit `andThen` takeWhile isDigit `andThen` "n"

nonDecimalIntegerLiteral = binaryIntegerLiteral <|> octalIntegerLiteral <|> hexIntegerLiteral

decimalLiteral =
  decimalIntegerLiteral
    `andThen` "."
    `andThen` takeWhile isDigit
    `andThen` option "" exponentPart
    <|>       "."
    `andThen` takeWhile1 isDigit
    `andThen` option "" exponentPart
    <|>       decimalIntegerLiteral
    `andThen` option "" exponentPart

decimalIntegerLiteral = "0" <|> nonZeroDigit `andThen` takeWhile isDigit

nonZeroDigit = T.singleton <$> satisfy (\c -> c /= '0' && isDigit c)

exponentPart = exponentIndicator `andThen` signedInteger

exponentIndicator = "e" <|> "E"

signedInteger = option "" ("+" <|> "-") `andThen` takeWhile1 isDigit

binaryIntegerLiteral = ("0b" <|> "0B") `andThen` takeWhile1 (\c -> c == '0' || c == '1')

octalIntegerLiteral = ("0o" <|> "0O") `andThen` takeWhile1 isOctDigit

hexIntegerLiteral = ("0x" <|> "0X") `andThen` takeWhile1 isHexDigit
